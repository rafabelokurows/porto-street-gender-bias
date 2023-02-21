# Identifying gender bias in Porto's street names

##  Description
Most cities have a long-standing gender bias towards men when naming streets, squares, gardens, and other public places. While there is a somewhat recent trend to include more women when defining city names, most street names are well-established and hard to change.
I wanted to at least a little bit on the subject starting with quantifying the gender bias where I live: Porto, Portugal.

## Methodology:  

Street names obtained from the Post office department of Portugal (CTT)
Gender of each name determined by:
  1. Query to Wikipedia attributes using [tidywikidatar](https://edjnet.github.io/tidywikidatar/) R 📦
  2. Gender predicting function present in [genderBR](https://github.com/meirelesff/genderBR) R 📦
  
Obs: some manual editing was necessary due to ambiguous and foreign names

![image](https://user-images.githubusercontent.com/55976107/205623899-bf2dbb53-7f9e-4669-a7a1-8208cfff3507.png)

Out of 1041 street names that can be attributed to persons, only 130 (12.49%) are women names.

## Roadmap

- [ ] Contacting Porto's city hall to check if there's some form of organized data on when each street name was awarded and for whom
- [ ] Updating plots, tables and maps with latest street names review
- [ ] Scrape also Wikipedia Link whenever there's a Wikipedia page on a person whose name is on a Porto street
