import math
import statistics
from typing import List, Optional, Union

Number = Union[int, float]

def arith_mean(xs: List[Number]) -> float:
    """Compute the arithmetic mean of a list."""
    return statistics.mean(xs) if xs else 0

def median(xs: List[Number]) -> Optional[float]:
    """Compute the median of a list."""
    return statistics.median(xs) if xs else None

def deviations(xs: List[Number]) -> List[float]:
    """Compute the deviations from the mean."""
    if not xs:
        return []
    mean = arith_mean(xs)
    return [x - mean for x in xs]

def mean_abs_dev(xs: List[Number]) -> Optional[float]:
    """Compute the Mean Absolute Deviation (MAD)."""
    devs = deviations(xs)
    return sum(map(abs, devs)) / len(xs) if xs else None

def variance(xs: List[Number]) -> Optional[float]:
    """Compute the variance (population variance)."""
    return statistics.pvariance(xs) if xs else None

def std_dev(xs: List[Number]) -> Optional[float]:
    """Compute the standard deviation (population standard deviation)."""
    return statistics.pstdev(xs) if xs else None

# Test Data
intl = list(range(1, 101))  # [1, 2, ..., 100]
fractl = [1.0, 1.5, 2.5]

# Compute and print results
datasets = {"intl": intl, "fractl": fractl}

for name, data in datasets.items():
    print(f"\nDataset: {name}")
    print(f"Arithmetic Mean: {arith_mean(data)}")
    print(f"Median: {median(data)}")
    print(f"Mean Absolute Deviation: {mean_abs_dev(data)}")
    print(f"Variance: {variance(data)}")
    print(f"Standard Deviation: {std_dev(data)}")