# Social Determinants of Health Indices – Geospatial Dashboard 

## Overview
The HSRC Geospatial Dashboard is an internal platform for exploring geospatial data curated by the SingHealth Health Services Research Centre (HSRC). It visualises social determinants of health (SDOH) indices across Singapore’s planning areas and provides quick lookup for 6-digit postal codes to retrieve relevant SDOH characteristics.

The underlying registry contains open-source geospatial datasets — including housing information, population census data, and land use details — curated and maintained by HSRC. Part of these datasets are used to create composite indices that help indicate SDOH based on place of residence.

**Indices available in the dashboard:**
- **SEDI** – Socioeconomic Disadvantage Index, based on [Earnest et al., 2015](https://www.sciencedirect.com/science/article/pii/S2211335515000558?via%3Dihub); updated for Singapore Census 2020 by Dr Yohei Okada (yohei_ok@duke-nus.edu.sg).  
- **SAI** – Socioeconomic Advantage Index, also based on [Earnest et al., 2015](https://www.sciencedirect.com/science/article/pii/S2211335515000558?via%3Dihub) and updated for Singapore Census 2020 by Dr Yohei Okada.  
- **SHI** – Singapore Housing Index, adapted from [Lim et al., 2021](https://equityhealthj.biomedcentral.com/articles/10.1186/s12939-021-01554-8).  
- **ESHI** – Enhanced Singapore Housing Index, an extension of SHI using public housing attributes such as price, rental status, and dwelling type; developed internally.
- **CDI** – Community Density Index, representing density based on housing unit counts and building height; developed internally.  
- **ACCESS** – Healthcare Access Index, representing proximity to public amenities (MRT/LRT stations, polyclinics, hospitals); internally.

Indices ESHI, CDI, and ACCESS are described in *Preliminary Development of Composite Geospatial Indices to Evaluate Social Determinants of Health on Asthma Outcomes* ([Quek et al., 2025](https://github.com/minerva79/geospatial_sdoh/blob/main/SDOH_Indices_draft.pdf)) and are currently under preparation for publication.



## Data & Technical Notes
- **Coordinate Reference System (CRS):** WGS84 (EPSG:4326)  
- **Polygon cleaning:** Performed using `sf::st_make_valid()` to ensure geometry validity.  
- **Index display:** For ESHI, CDI, and ACCESS, the dashboard shows either available text ranges or computed means.  
- This dashboard is for **exploratory use** only; data such as indices, hospital locations, and boundaries may be updated over time.

---

## Support & Feedback
For questions, data issues, or feature requests, please contact:  
**Adam Quek** – Analyst, Data Science, HSRC  
📧 **Email:** adamquek@nus.edu.sg
