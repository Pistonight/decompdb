use std::collections::BTreeMap;

use cu::pre::*;

use super::pre::*;

/// Create a map from name to all goffs that use that name
fn make_name_map(stage: &Stage1) -> cu::Result<BTreeMap<String, GoffSet>> {
    let mut typedefs: GoffMap<Vec<Goff>> = GoffMap::default();
    let mut goff2names: GoffMap<Vec<StructuredName>> = GoffMap::default();
    for (k, t) in &stage.types {
        match t {
            Type1::Prim(prim) => {
                let e = goff2names.entry(*k).or_default();
                e.push(
                    StructuredName::Name(NamespacedTemplatedName::new(
                        NamespacedName::prim(*prim)
                    ))
                );
            },
            Type1::Typedef(name, inner) => {
                // add the typedef name to the inner type
                let name = StructuredName::Name(name.clone());
                let e = goff2names.entry(*inner).or_default();
                e.push(name);
                // add the typedef link to duplicate the name later
                typedefs.entry(*inner).or_default().push(*k);
            }
            Type1::Enum(name, _) => {
                let Some(name) = name else {
                    // skip anonymous definitions
                    continue;
                };
                let name = StructuredName::Goff(name.clone(), vec![]);
                let e = goff2names.entry(*k).or_default();
                e.push(name);
            }
            Type1::Union(name, data) => {
                let Some(name) = name else {
                    // skip anonymous definitions
                    continue;
                };
                let name = StructuredName::Goff(name.clone(), data.template_args.clone());
                let e = goff2names.entry(*k).or_default();
                e.push(name);
            }
            Type1::Struct(name, data) => {
                let Some(name) = name else {
                    // skip anonymous definitions
                    continue;
                };
                let name = StructuredName::Goff(name.clone(), data.template_args.clone());
                let e = goff2names.entry(*k).or_default();
                e.push(name);
            }
            Type1::EnumDecl(name) |
            Type1::UnionDecl(name)|
            Type1::StructDecl(name) 
            => {
                let name = StructuredName::Name(name.clone());
                let e = goff2names.entry(*k).or_default();
                e.push(name);
            }
        }
    }

    // duplicate names to typedef goffs
    for (non_typedef_k, k_typedefs) in typedefs {
        let names = goff2names.get(&non_typedef_k).unwrap().clone();
        for k_typedef in k_typedefs {
            // sanity check: the initial loop does not add any typedefs
            // if output contains this typedef, it means this typedef
            // somehow points to 2 inner types
            cu::ensure!(!goff2names.contains_key(&k_typedef));
            goff2names.insert(k_typedef, names.clone());
        }
    }

    let mut permutater = StructuredNamePermutater::new(goff2names);
    let mut name2goffs = BTreeMap::<String, GoffSet>::new();
    for k in stage.types.keys().copied() {
        let names = cu::check!(permutater.permutated_string_reprs_goff(k), "failed to permutate names for type {k}")?;
        for name in names {
            name2goffs.entry(name).or_default().insert(k);
        }
    }

    Ok(name2goffs)
}
