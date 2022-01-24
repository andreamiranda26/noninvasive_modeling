## noninvasive_modeling 

- ## Creating an ABM to simulate counts of individuals through camera trapping and DNA analyses

**Andrea Miranda and Dr. Janna Willoughby**


We have outlined our model goals, parameters, and expected functions using the ODD protocol (Grimm et al. 2020):

*Model Overview*


We will use an agent based model to compare the number of uniquely identified individuals, assuming different error rates, using camera and genetic identification approaches. We use the camera trapping approach to simulate repeated samples over time at a fixed location whereas the DNA sampling samples a from more locations but is unlikely to sample a single location more than once. We have outlined our model goals, parameters, and expected functions using the ODD protocol (Grimm et al. 2020):

Overview
Purpose: This model will compare the number of individuals identified using two different monitoring approaches and assuming several different error rates.

Entities, state variables, and scale: Our models will have two entities: individuals that move around the landscape and the landscape itself. The individuals will be characterized by a unique ID and will be permitted to move at specified rates and directions. The landscape will include a grid of squares that will represent a unit area, scaled such that a square is equal to the distance an individual moves in a day. The landscape will also include sample locations, which will represent the location of cameras and DNA collection.

Process overview: Simulated individuals will be permitted to travel a specified number of grid cells each run in a randomly selected direction. At the end of each simulated "day", individuals that end in or pass through sample locations will be marked as ‘sampled’ for the methods explored in that particular model run. At the end of each simulated study, the number of individuals detected will be compared between methods, and all of these data will be stored for later analyses. 

Details
Initialization: The initial setup of the model will involve the gridded landscape which will contain individuals and sampling sites. The individuals will be initially placed randomly around in the landscape. 

Functions/subroutines: Our model will include functions for movement of individual, placement of cameras, overlaping of movement and camera locations, and DNA sampling.

**Expected Outcomes**

We expect that there will be a point where camera traps and DNA sampling detect similar number of individuals but that camera traps may be more efficient when errors are low. In this study, it is possible that we will not find any differences in individual counts between the two tested methods.

**Results**

Results can be found in the output folder and the Model Results document. 

