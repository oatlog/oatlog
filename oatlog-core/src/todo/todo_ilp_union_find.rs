// 2019:
//
// In Search of the Fastest Concurrent Union-Find Algorithm
// https://arxiv.org/abs/1911.06347
//
// 2018:
//
// Work‐efficient parallel union‐find
// https://onlinelibrary.wiley.com/doi/pdf/10.1002/cpe.4333?casa_token=uuLtWeNJmiwAAAAA:7d6oys5I3tL7om2e7HQ22iQ4ssnr_n4pF5__xdMO2takGhmhOrhEf0QfUUCx4UYqcGkgcJMFXMnZoVk
// => a set of unions can be reordered to be safe to perform in parallel.
//

// conceptually, performing ILP or SIMD [algorithm] is somewhat similar to parallel [algorithm]

// since we are single threaded, we are aware of all other union/find operations occurring.

use core::arch::x86_64::{
    __m256i, _mm256_cmpeq_epi32, _mm256_i32gather_epi32, _mm256_max_epu32, _mm256_min_epu32,
};
use std::arch::x86_64::_mm256_movemask_epi8;

// NOTE: it would be sound to perform path compression in parallel.
#[target_feature(enable = "avx2")]
fn simd_find(repr: &[u32], mut x: __m256i) -> __m256i {
    loop {
        // note: this gather could be masked.
        let x2 = unsafe { _mm256_i32gather_epi32(repr.as_ptr() as *const i32, x, 4) };

        let done_mask = _mm256_cmpeq_epi32(x, x2);
        if _mm256_movemask_epi8(done_mask) == -1 {
            break x;
        }
        x = x2;
    }
}

#[target_feature(enable = "avx2")]
fn simd_union(repr: &mut [u32], a: __m256i, b: __m256i) {
    let a = simd_find(repr, a);
    let b = simd_find(repr, b);

    let older = _mm256_min_epu32(a, b);
    let newer = _mm256_max_epu32(a, b);

    let older: [u32; 8] = unsafe { std::mem::transmute(older) };
    let newer: [u32; 8] = unsafe { std::mem::transmute(newer) };

    // since we always overwrite newer, the hazard occurs whenever newer is non root.
    // and that can only occur if values in newer alias with each-other.
    // so one could do a hazard check for all pairs of values in newer instead.

    // hazards should not occur often if the source of unions is vectorized exection.

    for i in 0..8 {
        // we don't really care if older is canonical here.
        if unsafe { *repr.get_unchecked(newer[i] as usize) } == newer[i] {
            *unsafe { repr.get_unchecked_mut(newer[i] as usize) } = older[i];
        } else {
            fallback_union(repr, newer[i], older[i])
        }
    }
}

// in "normal" workloads this basically never happens, we will see what happens with e-graphs.
#[cold]
fn fallback_union(repr: &mut [u32], mut a: u32, mut b: u32) {
    while a != repr[a as usize] {
        a = repr[a as usize];
    }
    while b != repr[b as usize] {
        b = repr[b as usize];
    }
    let (older, newer) = (u32::min(a, b), u32::max(a, b));
    repr[newer as usize] = older;
}

// https://hal.science/hal-02049029/document
