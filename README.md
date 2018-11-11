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
catkin_make                               
source ~/my_ws/devel/setup.bash
```

## known Problems

Sometimes giskard_msgs needs an extra build. do this with:

```
catkin_make giskard_msgs_generate_messages
```

## starting this package

- open commandline 
- roslaunch knowrob_refills knowrob_refills.launch 
- roslaunch refills_cram refills_cram.launch 

- then load this package into your roslisp_repl. 
- press ,
- type 'r-l-s' then 'tab'
- return
- type 'refills_cram' then return
- type 'refills-cram' then return
- The package gets compiled now
- execute the main-method with '(refills-cram:main)'
- Main method will now load in the shelfes and start the actionserver
