
use super::pre::*;

/// Mark and sweep garbage collection algorithm
pub fn mark_and_sweep<T, F : Fn(&T, Goff, &mut GoffSet)>(
    mut marked: GoffSet,
    map: &mut GoffMap<T>,
    marker: F
) {
    for (k, t) in &*map {
        marker(t, *k, &mut marked);
    }
    map.retain(|k, _| marked.contains(k));
}
