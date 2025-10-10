use cu::pre::*;

use super::super::bucket::GoffBuckets;
use super::pre::*;

pub fn run_stage2_parallel(mut stage: Stage1) -> cu::Result<Stage1> {
    cu::check!(
        super::merge_by_name(&mut stage),
        "merge_by_name failed for {}",
        stage.name
    )?;
    if stage.name.contains("PauseMenuDataMgr") {
        cu::print!("{:#?}", stage.types);
    }
    Ok(stage)
}

pub async fn run_stage2_serial(mut stages: Vec<Stage1>) -> cu::Result<Stage1> {
    cu::ensure!(!stages.is_empty(), "no CUs to merge");
    let stage = {
        let total = stages.len() - 1;
        let bar = cu::progress_bar(total, "stage1 -> stage2: merging types");
        let pool = cu::co::pool(-1);
        let mut handles = Vec::with_capacity(total / 2 + 1);
        while let Some(handle) = spawn_task(&mut stages, &pool) {
            handles.push(handle);
        }

        let mut count = 0;
        let mut set = cu::co::set(handles);
        while let Some(result) = set.next().await {
            let merged = result??;
            count += 1;
            cu::progress!(&bar, count);
            stages.push(merged);
            if let Some(handle) = spawn_task(&mut stages, &pool) {
                set.add(handle);
            }
        }

        let mut stage = stages.into_iter().next().unwrap();

        let mut marked = GoffSet::default();
        for symbol in stage.symbols.values() {
            symbol.mark(&mut marked);
        }
        super::super::garbage_collector::mark_and_sweep(marked, &mut stage.types, Type1::mark);
        cu::progress_done!(&bar, "stage2: merged into {} types", stage.types.len());
        stage
    };

    // cu::print!("{:#?}", stage.types);
    //
    let mut enum_count = 0;
    let mut union_count = 0;
    let mut struct_count = 0;
    let mut enum_decl_count = 0;
    let mut union_decl_count = 0;
    let mut struct_decl_count = 0;
    for t in stage.types.values() {
        match t {
            Type1::Prim(_) => {}
            Type1::Enum(_, _, _) => {
                enum_count += 1;
                cu::print!("{t:#?}");
            }
            Type1::Union(_, _, _) => union_count += 1,
            Type1::UnionDecl(_, _) => union_decl_count += 1,
            Type1::Struct(_, _, _) => struct_count += 1,
            Type1::EnumDecl(_, _) => {
                enum_decl_count += 1;
                // cu::print!("{t:#?}");
            }
            Type1::StructDecl(_, _) => {
                struct_decl_count += 1;
                // cu::print!("{t:#?}");
            }
        }
    }
    cu::print!("enum_count: {enum_count}");
    cu::print!("union_count: {union_count}");
    cu::print!("struct_count: {struct_count}");
    cu::print!("enum_decl_count: {enum_decl_count}");
    cu::print!("union_decl_count: {union_decl_count}");
    cu::print!("struct_decl_count: {struct_decl_count}");

    Ok(stage)
}

fn spawn_task(stages: &mut Vec<Stage1>, pool: &cu::co::Pool) -> Option<cu::co::Handle<cu::Result<Stage1>>> {
    if stages.len() <= 1 {
        return None;
    }
    let unit_a = stages.pop().unwrap();
    let unit_b = stages.pop().unwrap();
    let handle = pool.spawn(async move {
        let mut merged = unit_a.merge(unit_b)?;
        cu::check!(super::merge_by_name(&mut merged), "merged merge_by_name failed")?;
        cu::Ok(merged)
    });
    Some(handle)
}
