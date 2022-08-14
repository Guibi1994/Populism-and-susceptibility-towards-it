# Final data bases catalog and dictonaries

The data here aims to give an interested audience a minimum data structure to reproduce our work and validate the team's findings.

## 01_ideological_sourvey_colombia_2020.csv

This database contains the anonymized reports from the 2020 ideological survey conducted by the research team. In total, we collected 221 valid responses. The survey had two components. 1) we asked participants to evaluate on a scale from 1 to 9 (where one was far left and nine far right) all the candidates from Bogota's (Colombia) last four mayor elections; 2) we asked participants to evaluate the ideological scale of the leading 16 Colombian political parties.

| variable                            | description                                                                                                     |
|-------------------------------------|-----------------------------------------------------------------------------------------------------------------|
| Timestamp                           | Date and time were the sourvey was collected                                                                    |
| id_sourvey                          | Unique identifier of the participant                                                                            |
| respondent_gender                   | Participant's gender                                                                                            |
| respondent_birth_year               | Participant's year of birth                                                                                     |
| respondent_education_level          | Participant's education level                                                                                   |
| respondent_years_academic_jobs      | Participant's years o expirice working as a researcher, profesor or staf memeber in universities or think-tanks |
| respondent_particip_local_elections | Participant's number of participation in Bogota's mayor elections                                               |
| party                               | Name of the political party evaluated by the participant                                                        |
| idiological_value                   | Iidological value gave by the partipant to each political party                                                 |
