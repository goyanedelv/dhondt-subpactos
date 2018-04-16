# DHondt_subpact
Implementation in R of D'Hondt algorithm in the context of Chilean elections. 
This creates an internal iteration in which the D'Hondt algorithm is applied to the members of a same coalition but different parties.

### Requirements. 

It requires libraries: SciencesPo and dplyr.

### Inputs.

* Data: a dataset cointaining Name of the candidates, Coalition, District, Party and Votes. Preferred labels: Candidato, Pacto, Partido, votacion. This data is for all districts.
* District: number of the district.
* Seats: seats of the district.
