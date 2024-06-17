#!/usr/bin/env python

import os
import json
import os.path as osp
import importlib.util
import argparse

from typing import Dict, Callable, List, Any, TypeAlias

DatasetType: TypeAlias = List[List[Any]]


def import_module_from_path(file_path: str, module_name: str):
    spec = importlib.util.spec_from_file_location(module_name, file_path)
    module = importlib.util.module_from_spec(spec)  # type: ignore
    spec.loader.exec_module(module)  # type: ignore
    return module


def get_helper(dir_path: str):
    module = import_module_from_path(osp.join(dir_path, "eval.py"), "eval")
    return module.Helper()


def load_datasets(data_path: str) -> Dict[str, DatasetType]:
    datasets = {}
    datasets_path = os.listdir(data_path)
    for filename in datasets_path:
        file_path = osp.join(data_path, filename)
        with open(file_path, "r") as f:
            dataset: DatasetType = json.load(f)
            datasets[osp.basename(osp.splitext(file_path)[0])] = dataset

    return datasets


def get_functions(dir_path: str) -> Dict[str, Callable]:
    functions = {}
    for filename in os.listdir(dir_path):
        if filename.endswith(".py"):
            module_name = filename[:-3]
            file_path = osp.join(dir_path, filename)
            module = import_module_from_path(file_path, module_name)

            if hasattr(module, "__all__"):
                funcs = module.__all__
                for func_name in funcs:
                    func = getattr(module, func_name)
                    functions[osp.basename(osp.splitext(file_path)[0])] = func
    return functions


def test_functions(
    datasets: Dict[str, DatasetType], funcs: Dict[str, Callable], dir_path: str
) -> None:
    problems = list(datasets.keys())
    for problem in problems:
        dataset = datasets[problem]
        impl = funcs[problem]
        for test_case in dataset:
            helper = get_helper(dir_path)
            inputs = [
                helper.constructor(test_case_element)
                for test_case_element in test_case[:-1]
            ]
            func_output = impl(*inputs)
            if not isinstance(func_output, (int, bool)):
                func_output = helper.destructor(func_output)
            truth = test_case[-1]
            assert (
                func_output == truth
            ), f"""error in problem no.{problem}:
                Test Case: {test_case[0]}
                Got: {func_output}
                Expected: {truth}"""


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data_path",
        type=str,
        default="data/",
        help="path to the data directory, should contain csv files starting with leetcode problem number",
    )
    parser.add_argument(
        "--dir_path",
        type=str,
        default="python/",
        help="path to the directory, containing solutions for various problems",
    )
    args = parser.parse_args()
    # 1. Load Datasets
    datasets = load_datasets(args.data_path)

    # 2. Get Functions
    funcs = get_functions(args.dir_path)

    # 3. Test Functions
    test_functions(datasets, funcs, args.dir_path)
