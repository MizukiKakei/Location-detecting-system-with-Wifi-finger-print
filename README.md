# Location-detecting-system-with-Wifi-finger-print

### Objects and goals of this project
This  project is about creating system to navigate people inside the campus. Since GPS usually does not work well inside the building, we need to use other systems. Wifi finger print system can be one of the solution. 
520 Wifi spots were set in this campus and people used phones to observe the signals with these multiple Wifi spots (WAPs). 
The strong signal between phone and WAP also represents that the distance between phone and WAP is short. Thus by detecting the signals, the location can be predicted. 
The goal of this work is finding the machine learning model with high accuracy for Longitude, Latitude, Floor and Building.  

### Structure of Campus
- This campus consists of 3 buildings (building 0, 1 and 2).
- Building 0 has 5 floors (0 – 4th floor) and building 1 and 2 have 4 floors (0 – 3rd floor). 
- The image right shows the Map of this campus.

### Data set 
Two data sets of Training data and Validation data were provided. Training data obtained 19937 rows and validation data obtained 1111 rows. Both data set included 529 columns with 

-Location : Floor, Longitude, Latitude, Building, Space ID, Relative  position with respect to Space ID
-User Information : User ID, Phone ID 
-Time Stamp

### Methods of Analysis
6 ways of analysis were conducted. 
There were 2 different preprocesses (PCAs and No PCAs). For the classification of floor and building and the regression of longitude and latitude, 3 prediction models (KNN, SVM and Random forest) were applied. 

### Attached file description



