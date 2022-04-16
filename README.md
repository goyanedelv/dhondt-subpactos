# dHondt_Chile
Implementation in R of D'Hondt algorithm in the context of Chilean elections.

This creates an internal iteration in which the D'Hondt algorithm is applied to the members of a same coalition but different parties.

Also implemented `dhondt_Chile_general` which is a version "blind" to parties (it does not run the "second iteration") and `dhondt_Chile_with_threshold` that requires candidate to meet a certain threshold in order to be elected.

### Requirements

It requires libraries: `electoral` and `dplyr`.

### Inputs (of dHondt_Chile)

* Data: a dataset cointaining Name of the candidates, Coalition, District, Party and Votes. Preferred labels: Candidato, Pacto, Partido, votacion. This data is for all districts.
* District: number of the district.
* Seats: seats of the district.

### Output

* A data frame containing the elected members of the district.
