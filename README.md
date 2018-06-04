# refills_cram

## install instructions
```
source /opt/ros/kinetic/setup.bash 
mkdir -p ~/my_ws/src 
cd ~/my_ws
catkin init
cd src
wstool init  
wstool merge https://raw.githubusercontent.com/kstoermer/refills_cram/master/rosinstall/catkin.rosinstall
wstool update 
rosdep install --ignore-src --from-paths . 
cd ..                                      
catkin build                               
source ~/my_ws/devel/setup.bash
```
