# reciprocity_and_nepotism_in_superb_starlings

Data and analysis scripts for "A cryptic role for reciprocal helping in a cooperatively breeding bird"

**Authors**: [Alexis D. Earl](https://scholar.google.com/citations?hl=en&user=Yc4sb7cAAAAJ&view_op=list_works&sortby=pubdate), [Gerald G. Carter](https://scholar.google.com/citations?user=jtRkzp0AAAAJ&hl=en&oi=sra), [Shailee S. Shah](https://scholar.google.com/citations?hl=en&user=0lmbv34AAAAJ&view_op=list_works&sortby=pubdate), [Arden G. Berlinger](https://www.linkedin.com/in/arden-berlinger-38735b156/?originalSubdomain=uk), & [Dustin R. Rubenstein](https://scholar.google.com/citations?user=Wsh4RjcAAAAJ&hl=en&oi=sra)

**Abstract**: 
Identifying the mechanisms that underlie cooperation is a fundamental question in biology, yet it is one that remains surprisingly unresolved in the majority of social vertebrates. The most complex form of cooperation in vertebrates—-including in humans-—occurs in cooperative breeders, where helpers forego reproduction and assist in raising the young of others, typically relatives. Not all cooperative societies, however, are kin-based—-nearly half of all avian and mammalian cooperative breeders form mixed-kin societies, much like those of humans. Kin selection in mixed-kin societies occurs when individuals gain indirect fitness from nepotism (the preferential helping of relatives), but helpers also frequently assist nonkin, highlighting a potential role for direct fitness in stabilizing cooperative societies. Using a 20-year study of superb starlings (Lamprotornis superbus), we examined how direct and indirect fitness jointly influence helping behavior. Although we detected nepotism, nonkin helping was common despite opportunities to aid kin. Surprisingly, specific pairs maintained long-term reciprocal helping relationships by swapping social roles across their lifetimes-—a subtle pattern of reciprocity requiring decades of observation to detect. The importance of direct and indirect fitness varied with helpers’ sex and dispersal history. Given the frequency of nonkin helping and the occurrence of reciprocal helping among both kin and nonkin, helping behavior in superb starlings appears to be more influenced by direct fitness than by kin selection. By uncovering a cryptic yet crucial role of long-term reciprocal helping, we suggest that reciprocity is an underappreciated mechanism promoting the stability of cooperatively breeding societies.

# Folders
1. **data**

   Cleaned data can be found here.

- ```daily_helping.csv``` contains information on observations of superb starlings. Columns include: (A) breeding_season (the year followed by whether it was the long rains (LR) or short rains (SR) rainy season), (B) date (month/day/year), (C) group (social group identity), (D) helper (individual identity of the helper observed at the nest), (E) father (identity of the father at that nest), (F) mother (identity of the mother at that nest), (G)	nest (nest identity), (H) help, (I) sample.duration, (J)	mom.dyad, (K) dad.dyad, (L) kinship.mom, (M)	kinship.dad, (N) kinship.max, (O) microsat.kinship.mom, (P) microsat.kinship.dad, (Q) microsat.kinship.max, (R) helper.sex, (S) helper.dispersal,	(T) helping.rate, (U) reciprocal.help.mom, (V) reciprocal.help.dad, (W) reciprocal.help.max, (X) reciprocal.help.

* ```dyads.csv``` contains information on the relationships between pairs of superb starlings, i.e., the "helper" and "receiver" (the breeder receiving help). Columns include: (A) identity of the helper, (B) identity of the receiver of help (i.e., the breeder), (C) sex (M for male and F for female) of the helper, (D) "helper.type", which indicates the helper's dispersal history (immigrant or resident) and their sex, (E) receiver (breeder) sex (M for male and F for female), (F) kinship, which indicates the pairwise relatedness of the dyad, (G) help.rate, which indicates the rate of helping from the helper to the receiver (i.e., mean of dyadic daily helping divided by daily sample duration), (H) dyad (the identity of the helper and the identity of the receiver), (I) reciprocal.dyad (the identities of the individuals in the dyad but in the reverse order), (J) udyad (undirected dyad label), (K) reciprocal.help.rate, which indicates the help rate from the individual who is currently the receiver to the current helper

3. **code**

   Scripts for all analyses can be found here.

# Contact
If you have any questions or suggestions please feel free to open an issue on this repo or email me at ade2102@columbia.edu.
