cmake_minimum_required(VERSION 3.16)

project(dsm LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# --- Dépendances ---
find_package(Eigen3 REQUIRED)
find_package(pybind11 REQUIRED)

# =========================================================
# Module Python (DSM backend)
# =========================================================
pybind11_add_module(dsm_cpp
    cpp/dsm_pybind.cpp
    cpp/dsm.cpp
)

target_link_libraries(dsm_cpp
    PRIVATE Eigen3::Eigen
)

target_compile_definitions(dsm_cpp PRIVATE PYBIND)

target_compile_options(dsm_cpp PRIVATE
    -Wall -Wextra -Wpedantic
)


