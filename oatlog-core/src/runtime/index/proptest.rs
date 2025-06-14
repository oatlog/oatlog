use crate::runtime::{HashMap, IndexedSortedList};
use itertools::Itertools as _;
use proptest::prelude::*;
use std::{fmt::Debug, hash::Hash};

// to make sure that tests are not accidentally run without debug asserts.
#[should_panic]
#[test]
fn debug_assert_is_enabled() {
    debug_assert!(false);
}

proptest! {
    #[test]
    fn test_dedup_suffix((v, idx) in any::<Vec<u32>>()
        .prop_filter("len>0", |x| !x.is_empty())
        .prop_map(|mut x| { x.sort_unstable(); x })
        .prop_perturb(|x, mut rng| (x.clone(), rng.gen_range(0..x.len())))
    ) {
        let v: Vec<u32> = v;
        let idx: usize = idx;

        let current = {
            let mut v = v.clone();
            super::dedup_suffix(&mut v, idx);
            v
        };
        let expected = {
            let prefix = &v[0..idx];
            let suffix = &v[idx..];
            prefix.iter().copied().chain(suffix.iter().copied().dedup()).collect_vec()
        };
        assert_eq!(expected, current);
    }
}

#[derive(Clone, Debug)]
enum Action<Key, Value> {
    Reconstruct(Vec<(Key, Value)>),
    Iter(Key),
    IterAll,
}

const ACTIONS: usize = 20;
const SIZE: usize = 100;
const ELEM: u32 = 16;

fn actions_strategy<Key, Value, KeyGen, ValueGen>(
    key: KeyGen,
    value: ValueGen,
) -> impl Strategy<Value = Vec<Action<Key, Value>>>
where
    KeyGen: Strategy<Value = Key> + Clone + 'static,
    ValueGen: Strategy<Value = Value> + Clone + 'static,
    Key: Debug + Clone + 'static + Ord,
    Value: Debug + Clone + 'static + Ord,
{
    // let action: BoxedStrategy<Action<Key, Value>> =
    //     proptest::sample::select(vec![iter, reconstruct, iter_all])
    //         .prop_flat_map(std::convert::identity)
    //         .boxed();

    proptest::collection::vec(
        proptest::prop_oneof![
            key.clone().prop_map(Action::Iter),
            proptest::collection::vec((key, value), 0..SIZE)
                .prop_map(|mut x| {
                    x.sort();
                    x.dedup();
                    x
                })
                .prop_map(Action::Reconstruct),
            proptest::strategy::Just(Action::IterAll),
        ],
        0..ACTIONS,
    )
}

#[rustfmt::skip]
proptest! {
    // #![proptest_config(ProptestConfig::with_cases(1000))]
    #[test] fn test_index_1_0(actions in actions_strategy(0..ELEM, Just(()))) { index_test(actions); }
    #[test] fn test_index_0_1(actions in actions_strategy(Just(()), 0..ELEM)) { index_test(actions); }
    #[test] fn test_index_1_1(actions in actions_strategy(0..ELEM, 0..ELEM)) { index_test(actions); }
    // #[test] fn test_index_1_2(actions in actions_strategy((0..ELEM), (0..ELEM, 0..ELEM))) { index_test(actions); }
    // #[test] fn test_index_1_3(actions in actions_strategy((0..ELEM), (0..ELEM, 0..ELEM, 0..ELEM))) { index_test(actions); }
    // #[test] fn test_index_2_1(actions in actions_strategy((0..ELEM, 0..ELEM), (0..ELEM))) { index_test(actions); }
    // #[test] fn test_index_2_2(actions in actions_strategy((0..ELEM, 0..ELEM), (0..ELEM, 0..ELEM))) { index_test(actions); }
    // #[test] fn test_index_2_3(actions in actions_strategy((0..ELEM, 0..ELEM), (0..ELEM, 0..ELEM, 0..ELEM))) { index_test(actions); }
    // #[test] fn test_index_3_1(actions in actions_strategy((0..ELEM, 0..ELEM, 0..ELEM), (0..ELEM))) { index_test(actions); }
    // #[test] fn test_index_3_2(actions in actions_strategy((0..ELEM, 0..ELEM, 0..ELEM), (0..ELEM, 0..ELEM))) { index_test(actions); }
    // #[test] fn test_index_3_3(actions in actions_strategy((0..ELEM, 0..ELEM, 0..ELEM), (0..ELEM, 0..ELEM, 0..ELEM))) { index_test(actions); }
}

fn index_test<
    Key: Copy + Clone + Ord + Hash + Default + Debug,
    Value: Copy + Clone + Ord + Hash + Default + Debug,
>(
    actions: Vec<Action<Key, Value>>,
) {
    let mut index1: IndexedSortedList<Key, Value> = Default::default();
    let mut index2: HashMap<Key, Vec<Value>> = Default::default();

    for action in actions {
        match action {
            Action::Reconstruct(items) => {
                let mut rows = items.clone();
                unsafe {
                    rows.sort_by_key(|&(key, _)| key);
                    index1.reconstruct(&mut rows, |(key, _)| key, |(_, value)| value);
                }

                index2.clear();
                for (key, value) in items {
                    index2.entry(key).or_default().push(value);
                }
            }
            Action::Iter(key) => {
                let mut items1: Vec<_> = index1.iter(key).collect();
                items1.sort();
                let mut items2: Vec<_> = index2.get(&key).cloned().unwrap_or(vec![]);
                items2.sort();
                assert_eq!(items1, items2);
            }
            Action::IterAll => {
                let mut items1: Vec<_> = index1.iter_key_value().collect();
                items1.sort();
                let mut items2: Vec<_> = index2
                    .iter()
                    .flat_map(|(&key, value)| value.iter().copied().map(move |value| (key, value)))
                    .collect();
                items2.sort();
                assert_eq!(items1, items2);
            }
        }
    }
}
