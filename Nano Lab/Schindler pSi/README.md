# ploting and simulation of lab class nano @ Prof. Schindler pSi
These scripts simplify the evaluation of the data provided in Moodle.
As well as a simulation for comparing the theoretical and experimental reflection of a Bragg mirror.

Please copy your data into the .csv files or adapt the code to your data structure.

## R-script_Reflektrometrie.R

Creates a plot of reflection vs wavelenght. The data provided.
NOTE: the .xy file was converted to a .csv file.

## R-script_Profilometrie.R

Creates a plots of movement x vs movement in y. The data provided.
NOTE: the .xy file was converted to a .csv file.

## R-script_Profilometrie_lens.R

Creates a plots of movement x vs movement in y. The data provided.
NOTE: the .xy file was converted to a .csv file.

## R-script_elipsometer.R

Creates a plots of movement x vs refractive index and thickness d. The data provided.
This scipt also tries to a mean for the etched part and unetched part to get a good value for the step.
Thies will be also put in a plot.
NOTE: the .xy file was converted to a .csv file.

## R-script_Simulation.R

This script simulates a Bragg mirror. But you can also simulate other combinations of layers with different refractive indices. Besides the refractive index and thickness, the angle of incidence is also taken into account.
A TMM (Transfer Matrix Method with the Fressnel equations) is used to get the reflection.
Please fill in the needed values for you simulation at the begining of the code

## All rights reserved 
Sebastian Stadler 
Germany
