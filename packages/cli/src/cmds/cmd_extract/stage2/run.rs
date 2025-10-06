use cu::pre::*;

use super::super::bucket::GoffBuckets;
use super::pre::*;

pub fn run_stage2_parallel(mut stage: Stage1) -> cu::Result<Stage1> {
    cu::check!(super::merge_by_name(&mut stage), "merge_by_name failed for {}", stage.name)?;
    if stage.name.contains("PauseMenuDataMgr") {
        cu::print!("{:#?}", stage.types);
    }
    Ok(stage)
}

pub async fn run_stage2_serial(mut stages: Vec<Stage1>) -> cu::Result<Stage1> {
    cu::ensure!(!stages.is_empty(), "no CUs to merge");
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
        count+=1;
        cu::progress!(&bar, count);
        stages.push(merged);
        if let Some(handle) = spawn_task(&mut stages, &pool) {
            set.add(handle);
        }
    }

    let stage = stages.into_iter().next().unwrap();
    let type_count = stage.types.len();
    cu::progress_done!(&bar, "stage2: merged into {type_count} types");

    // cu::print!("{:#?}", stage.types);

    Ok(stage)

}

fn spawn_task(
    stages: &mut Vec<Stage1>,
    pool: &cu::co::Pool
) -> Option<cu::co::Handle<cu::Result<Stage1>>>{
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
