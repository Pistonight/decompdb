use super::pre::*;

/// Mark and sweep garbage collection algorithm
pub fn mark_and_sweep<T, F: Fn(&T, Goff, &mut GoffSet)>(always_marked: GoffSet, map: &mut GoffMap<T>, marker: F) {
    loop {
        let before = map.len();
        let mut marked = always_marked.clone();
        for (k, t) in &*map {
            marker(t, *k, &mut marked);
        }
        map.retain(|k, _| marked.contains(k));
        if map.len() == before {
            return;
        }
    }
}
