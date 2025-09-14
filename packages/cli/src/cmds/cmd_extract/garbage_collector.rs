
use super::pre::*;

/// Mark and sweep garbage collection algorithm
pub fn mark_and_sweep<T, F : Fn(&T, &mut GoffSet)>(
    map: &mut GoffMap<T>,
    marker: F
) {
    let mut marked = GoffSet::default();
    for t in map.values() {
        marker(t, &mut marked);
    }
    map.retain(|k, _| marked.contains(k));
}
