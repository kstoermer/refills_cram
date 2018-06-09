cmake_minimum_required(VERSION 2.8.3)
project(refills_cram)
find_package(catkin REQUIRED COMPONENTS
  roslisp
  std_msgs
  message_generation
)


catkin_package(CATKIN_DEPENDS roslisp std_msgs message_runtime)

add_lisp_executable(main
refills-cram refills-cram:main)
