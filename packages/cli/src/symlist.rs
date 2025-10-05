use std::{collections::BTreeMap, sync::Arc};

use cu::pre::*;

use crate::{config::CfgCsv, demangler::Demangler};

/// Data structure that lists symbols and their addresses
#[derive(Default)]
pub struct SymbolList {
    map: BTreeMap<String, u32>,
}

impl SymbolList {
    pub fn len(&self) -> usize {
        self.map.len()
    }
    pub fn load_data(&mut self, config: &CfgCsv) -> cu::Result<()> {
        let map = cu::check!(load_symbol_csv(config), "failed to load data symbols")?;
        self.map.extend(map);
        Ok(())
    }
    pub async fn load_func(&mut self, config: &CfgCsv, demangler: Arc<Demangler>) -> cu::Result<()> {
        let map = cu::check!(load_symbol_csv(config), "failed to load func symbols")?;

        // fabricate D1/D2 and C1/C2 if either is missing
        let pool = cu::co::pool(-1);
        let mut handles = vec![];
        for (symbol, addr) in &map {
            let addr = *addr;
            let demangler = Arc::clone(&demangler);
            let symbol = symbol.to_string();
            let handle = pool.spawn(async move {
                let result = get_all_possible_symbols(&symbol, &demangler)?;
                cu::Ok((result, addr))
            });
            handles.push(handle);
        }
        let total = map.len();
        let bar = cu::progress_bar(total, "filling ctor/dtor symbols");
        let mut i = 0;
        let mut set = cu::co::set(handles);
        while let Some(result) = set.next().await {
            i += 1;
            cu::progress!(&bar, i);
            let (symbols, addr) = cu::check!(result.flatten(), "failed to get all possible symbols")?;
            match symbols {
                PossibleSymbols::Only(_) => continue,
                PossibleSymbols::Dtor12(d1, d2) => {
                    if !map.contains_key(&d1) {
                        self.map.insert(d1, addr);
                    }
                    if !map.contains_key(&d2) {
                        self.map.insert(d2, addr);
                    }
                }
                PossibleSymbols::Ctor12(c1, c2) => {
                    if !map.contains_key(&c1) {
                        self.map.insert(c1, addr);
                    }
                    if !map.contains_key(&c2) {
                        self.map.insert(c2, addr);
                    }
                }
            }
        }
        self.map.extend(map);
        Ok(())
    }
    /// Get the address of symbol
    pub fn get_address(&self, symbol: &str) -> Option<u32> {
        self.map.get(symbol).copied()
    }
}

pub fn load_symbol_csv(config: &CfgCsv) -> cu::Result<BTreeMap<String, u32>> {
    let content = cu::fs::read_string(&config.path)?;
    let address_column = config.address_column;
    let symbol_column = config.symbol_column;

    let mut map = BTreeMap::default();

    for (i, line) in content.lines().enumerate().skip(config.skip_rows) {
        let row = i + 1;
        let parts = line.split(',').collect::<Vec<_>>();
        let address = cu::check!(
            parts.get(address_column),
            "failed to get address column at row {row} (address_column={address_column})"
        )?;
        let address = cu::check!(cu::parse::<u64>(address), "failed to parse address at row {row}")?;
        let rel_address = cu::check!(
            address.checked_sub(config.base_address),
            "address is less than base address at row {row}"
        )?;
        cu::ensure!(
            rel_address <= u32::MAX as u64,
            "relative address at row {row} is too big, this is likely wrong"
        );

        let symbol = cu::check!(
            parts.get(symbol_column),
            "failed to get symbol column at row {row} (symbol_column={symbol_column})"
        )?;
        let symbol = symbol.trim();

        if symbol.is_empty() {
            continue;
        }

        map.insert(symbol.to_string(), rel_address as u32);
    }

    Ok(map)
}

fn get_all_possible_symbols(symbol: &str, demangler: &Demangler) -> cu::Result<PossibleSymbols> {
    // demangle the symbol
    let demangled = demangler.demangle(symbol)?;
    if demangled == symbol {
        // not a mangled symbol
        return Ok(PossibleSymbols::Only(demangled));
    }
    if is_dtor(&demangled) {
        let positions = get_positions(symbol, false);
        cu::ensure!(
            !positions.is_empty(),
            "cannot find D0, D1 or D2 in mangled dtor symbol: {symbol}"
        );
        let mut buf = symbol.to_string();
        let mut good = None;
        for i in positions {
            // test d0
            set_str_byte(&mut buf, i, '0');
            let is_d0 = symbol == &buf;
            let Ok(demangled_d0) = demangler.demangle(&buf) else {
                continue;
            };
            if demangled_d0 != demangled {
                continue;
            }
            // test d1
            set_str_byte(&mut buf, i, '1');
            let Ok(demangled_d1) = demangler.demangle(&buf) else {
                continue;
            };
            if demangled_d1 != demangled {
                continue;
            }
            // test d2
            set_str_byte(&mut buf, i, '2');
            let Ok(demangled_d2) = demangler.demangle(&buf) else {
                continue;
            };
            if demangled_d2 != demangled {
                continue;
            }
            // good
            if is_d0 {
                // symbol is D0, D0 is the deleting dtor, which must be
                // different from D1/D2
                return Ok(PossibleSymbols::Only(symbol.to_string()));
            }
            good = Some(i);
            break;
        }
        let i = cu::check!(good, "failed to determine if dtor is D0/D1/D2: {symbol}")?;
        set_str_byte(&mut buf, i, '1');
        let d1 = buf.clone();
        set_str_byte(&mut buf, i, '2');
        return Ok(PossibleSymbols::Dtor12(d1, buf));
    }

    // might be ctor or regular function
    let positions = get_positions(symbol, true);
    if positions.is_empty() {
        return Ok(PossibleSymbols::Only(symbol.to_string()));
    }
    let mut buf = symbol.to_string();
    let mut good = None;
    for i in positions {
        // test c3
        set_str_byte(&mut buf, i, '3');
        let is_c3 = symbol == &buf;
        let Ok(demangled_c3) = demangler.demangle(&buf) else {
            continue;
        };
        if demangled_c3 != demangled {
            continue;
        }
        // test c1
        set_str_byte(&mut buf, i, '1');
        let Ok(demangled_c1) = demangler.demangle(&buf) else {
            continue;
        };
        if demangled_c1 != demangled {
            continue;
        }
        // test c2
        set_str_byte(&mut buf, i, '2');
        let Ok(demangled_c2) = demangler.demangle(&buf) else {
            continue;
        };
        if demangled_c2 != demangled {
            continue;
        }
        // good
        if is_c3 {
            // symbol is C3, which is allocating ctor
            return Ok(PossibleSymbols::Only(symbol.to_string()));
        }
        good = Some(i);
        break;
    }
    let Some(i) = good else {
        // probably false positives
        return Ok(PossibleSymbols::Only(symbol.to_string()));
    };
    set_str_byte(&mut buf, i, '1');
    let c1 = buf.clone();
    set_str_byte(&mut buf, i, '2');
    Ok(PossibleSymbols::Ctor12(c1, buf))
}

fn is_dtor(symbol: &str) -> bool {
    symbol.starts_with('~') || symbol.contains("::~")
}

fn get_positions(symbol: &str, for_ctor: bool) -> Vec<usize> {
    let mut positions = vec![];
    let mut prev = ' ';
    if for_ctor {
        for (i, c) in symbol.char_indices() {
            if prev != 'C' {
                prev = c;
                continue;
            }
            prev = c;
            match c {
                '1' | '2' | '3' => {
                    positions.push(i);
                }
                _ => {}
            }
        }
    } else {
        for (i, c) in symbol.char_indices() {
            if prev != 'D' {
                prev = c;
                continue;
            }
            prev = c;
            match c {
                '1' | '2' | '0' => {
                    positions.push(i);
                }
                _ => {}
            }
        }
    }
    positions
}

fn set_str_byte(s: &mut String, i: usize, b: char) {
    unsafe { s.as_bytes_mut()[i] = b as u8 }
}

enum PossibleSymbols {
    // D0, C3, or not dtor/ctor
    Only(String),
    // D1 and D2 might be the same function
    // and referred to differently in different places,
    // so if we detect a D1/D2, it could be either,
    // same for C1 and C2
    Dtor12(String, String), // D1, D2
    Ctor12(String, String), // C1, C2
}
