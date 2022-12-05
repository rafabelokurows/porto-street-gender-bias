# portoStreetNames

Identifying how biased is Porto's street naming convetion is towards men names. SPOILER: A LOT!  

![image](https://user-images.githubusercontent.com/55976107/205623899-bf2dbb53-7f9e-4669-a7a1-8208cfff3507.png)

Out of 1041 street names that can be attributed to persons, only 130 (12.49%) are women names.

Methodology:  

Street names obtained from Portugal postal codes base (Source: CTT)  

Gender determined by:
  1. Query to Wikipedia attributes using [tidywikidatar](https://edjnet.github.io/tidywikidatar/) R 📦
  2. Gender predicting function present in [genderBR](https://github.com/meirelesff/genderBR) R 📦
  
Obs: some manual editing was necessary due to unambiguous and foreign names
