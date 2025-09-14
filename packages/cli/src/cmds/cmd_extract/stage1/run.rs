
use cu::pre::*;

use super::pre::*;

pub fn run_stage1(mut stage0: Stage0) -> cu::Result<()> {
    cu::check!(super::resolve_enum_sizes(&mut stage0), "stage1: resolve_enum_sizes failed")?;
    cu::check!(super::clean_typedefs(&mut stage0), "stage1: clean_typedefs failed")?;
    cu::check!(super::flatten_trees(&mut stage0), "stage1: flatten_trees failed")?;

    Ok(())
}
