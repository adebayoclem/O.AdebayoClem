DROP TABLE [12monthECDS]

SELECT 
 [vtECDS].PheKey
  ,TOKEN_PERSON_ID
  ,[Y006_BBV_PID].[dbo].[vECDS_PID].[BIRTH_DATE]
  , [Y006_BBV_PID].[dbo].[vECDS_PID].[NHS_NUMBER]
  ,[Y006_BBV_PID].[dbo].[vECDS_PID].[POSTCODE]
  ,[PROVIDER_CODE_DERIVED]
  ,[SITE]
  ,[ARRIVAL_MONTH]
  ,[ARRIVAL_DATE]
  ,[ECDS].[dbo].[vtECDS].[FYEAR]
  ,[InvestigationIdx]
  ,[INVESTIGATION_DATE]
  ,[INVESTIGATION_CODE_Description]
  ,[AGE_AT_ARRIVAL]
    , CASE 
  WHEN [AGE_AT_ARRIVAL] between 0 and 15 THEN 'Under 16' 
  WHEN [AGE_AT_ARRIVAL] between 16 and 24 THEN '16-24' 
  WHEN [AGE_AT_ARRIVAL] between 25 and 34 THEN '25-34' 
  WHEN [AGE_AT_ARRIVAL] between 35 and 49 THEN '35-49' 
  WHEN [AGE_AT_ARRIVAL] between 50 and 64 THEN '50-64' 
  WHEN [AGE_AT_ARRIVAL] between 65 and 79 THEN '65-79' 
  WHEN [AGE_AT_ARRIVAL] between 80 and 130 THEN '80+' 
  ELSE null		
  END AS age_group
      , CASE 
  WHEN [AGE_AT_ARRIVAL] between 0 and 15 THEN 'Under 16' 
  WHEN [AGE_AT_ARRIVAL] between 16 and 44 THEN '16-44' 
  WHEN [AGE_AT_ARRIVAL] between 45 and 64 THEN '45-64' 
  WHEN [AGE_AT_ARRIVAL] between 65 and 130 THEN '65+' 
  ELSE null		
  END AS age_group2

  , case
    WHEN [ETHNIC_CATEGORY_Description] = 'White British' THEN 'White British' 
	    WHEN [ETHNIC_CATEGORY_Description] = 'Any other White background' THEN 'White Other' 
		    WHEN [ETHNIC_CATEGORY_Description] = 'White Irish' THEN 'White Other' 
			    WHEN [ETHNIC_CATEGORY_Description] = 'African' THEN 'Black African' 
				    WHEN [ETHNIC_CATEGORY_Description] = 'Caribbean' THEN 'Black Caribbean' 
				    WHEN [ETHNIC_CATEGORY_Description] = 'Any other Black background' THEN 'Black Other' 
					 WHEN [ETHNIC_CATEGORY_Description] = 'Indian' THEN 'Indian, Pakistani or Bangladeshi' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Bangladeshi' THEN 'Indian, Pakistani or Bangladeshi' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Pakistani' THEN 'Indian, Pakistani or Bangladeshi' 
				    WHEN [ETHNIC_CATEGORY_Description] = 'Chinese' THEN 'Asian Other' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Any other Asian background' THEN 'Asian Other' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Any other mixed background' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'White and Asian' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'White and Black African' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'White and Black Caribbean' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Any other ethnic group' THEN 'Other' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Not known e.g. unconscious' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = '<NA>' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = 'NA' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = '' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Not stated e.g. unwilling to state' THEN 'Unknown' 
    ELSE null		
  END AS ethnic_group
             
  ,[LSOA_2011]
  ,[LookupsShared].[dbo].[vSocioDemog_LSOA11].[IMD2019_Deciles_LSOA11_withinUTLA21]
  ,[ETHNIC_CATEGORY]
  ,[ETHNIC_CATEGORY_Description]
   ,[STATED_GENDER]
  ,[INTERPRETER_LANGUAGE_Description]
  ,[INTERPRETER_LANGUAGE_VALID]
  ,[PREFERRED_SPOKEN_LANGUAGE_Description]
  ,[PREFERRED_SPOKEN_LANGUAGE_VALID]
  ,[ACCOMMODATION_STATUS_Description]
  ,[ACCOMMODATION_STATUS_VALID]
  ,[DRUG_ALCOHOL_CODE_1_Description]


  , CASE 			
  WHEN [PROVIDER_CODE_DERIVED] in ('RRV','RP4','RAL','RAN','RAP','RRP','RP6','RKE','RNK') THEN 'NCL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('R1H','RF4','RWK','RQX','RAT')   THEN 'NEL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('RYJ','RT3','RQM','RKL','R1K','RV3','RAS','RYX') THEN 'NWL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ1','RJZ','RV5','RJ2','RPG')   THEN 'SEL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ7','RVR','RQY','RJ6','RAX','RPY')   THEN 'SWL'	
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ7','RVR','RQY','RJ6','RAX','RPY')   THEN 'SE'	
  ELSE 'Other'  		
  END AS London_Provider_ICS
  
  , CASE 
  WHEN [PROVIDER_CODE_DERIVED] in ('RTP') THEN 'UNIVERSITY HOSPITALS SUSSEX NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('R0A') THEN 'MANCHESTER UNIVERSITY NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('RM3') THEN 'SALFORD ROYAL NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('RXL') THEN 'BLACKPOOL TEACHING HOSPITALS NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('RRV') THEN 'UNIVERSITY COLLEGE LONDON HOSPITALS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RP4') THEN 'GREAT ORMOND STREET HOSPITAL FOR CHILDREN NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAL') THEN 'ROYAL FREE LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RP6') THEN 'MOORFIELDS EYE HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAN') THEN 'ROYAL NATIONAL ORTHOPAEDIC HOSPITAL NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAP') THEN 'NORTH MIDDLESEX UNIVERSITY HOSPITAL NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RRP') THEN 'BARNET, ENFIELD AND HARINGEY MENTAL HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RKE') THEN 'THE WHITTINGTON HOSPITAL NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RNK') THEN 'TAVISTOCK AND PORTMAN NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('R1H') THEN 'BARTS HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RF4') THEN 'BARKING, HAVERING AND REDBRIDGE UNIVERSITY HOSPITALS NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RWK') THEN 'EAST LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RQX') THEN 'HOMERTON UNIVERSITY HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAT') THEN 'NORTH EAST LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RYJ') THEN 'IMPERIAL COLLEGE HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RT3') THEN 'ROYAL BROMPTON & HAREFIELD NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RQM') THEN 'CHELSEA AND WESTMINSTER HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RKL') THEN 'WEST LONDON MENTAL HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RV3') THEN 'CENTRAL AND NORTH WEST LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAS') THEN 'THE HILLINGDON HOSPITALS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RYX') THEN 'CENTRAL LONDON COMMUNITY HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ1') THEN 'GUYS AND ST THOMAS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJZ') THEN 'KINGS COLLEGE HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RV5') THEN 'SOUTH LONDON AND MAUDSLEY NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ2') THEN 'LEWISHAM HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RPG') THEN 'OXLEAS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ7') THEN 'ST GEORGES HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RVR') THEN 'EPSOM AND ST HELIER UNIVERSITY HOSPITALS NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RQY') THEN 'SOUTH WEST LONDON AND ST GEORGES MENTAL HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ6') THEN 'CROYDON HEALTH SERVICES NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAX') THEN 'KINGSTON HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RPY') THEN 'THE ROYAL MARSDEN NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('R1K') THEN 'LONDON NORTH WEST UNIVERSITY HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RYR') THEN 'University Hospitals Sussex NHS Foundation Trust'
  ELSE 'Other'  		
  END AS Provider_derived

   , CASE 
  WHEN SITE = 'RAL26' THEN 'Yes'
  WHEN Site = 'RXL01' THEN 'Yes'
  WHEN site = 'RYJ02' THEN 'Yes'
  WHEN site = 'RQM01' THEN 'Yes'
  WHEN site = 'RJ611' THEN 'Yes'
  WHEN site = 'R1K04' THEN 'Yes'
  WHEN site = 'RVR50' THEN 'Yes'
  WHEN site = 'RAS01' THEN 'Yes'
  WHEN site = 'RQXM1' THEN 'Yes'
  WHEN site = 'RF4DG' THEN 'Yes'
  WHEN site = 'RJZ01' THEN 'Yes'
  WHEN site = 'RAX01' THEN 'Yes'
  WHEN site = 'R0A02' THEN 'Yes'
  WHEN site = 'R1HNH' THEN 'Yes'
  WHEN site = 'R0A66' THEN 'Yes'
  WHEN site = 'RAPNM' THEN 'Yes'
  WHEN site = 'R1K01' THEN 'Yes'
  WHEN site = 'RJZ30' THEN 'Yes'
  WHEN site = 'RJ231' THEN 'Yes'
  WHEN site = 'RF4QH' THEN 'Yes'
  WHEN site = 'RAL01' THEN 'Yes'
  WHEN site = 'E0A3H' THEN 'Yes'
  WHEN site = 'RJ701' THEN 'Yes'
  WHEN site = 'RVR05' THEN 'Yes'
  WHEN site = 'RYJ01' THEN 'Yes'
  WHEN site = 'RJ122' THEN 'Yes'
  WHEN site = 'R1H12' THEN 'Yes'
  WHEN site = 'RKEQ4' THEN 'Yes' 
  WHEN site = 'RRV03' THEN 'Yes'
  WHEN site = 'RJ224' THEN 'Yes'
  WHEN site = 'RQM91' THEN 'Yes'
  WHEN site = 'R1HKH' THEN 'Yes'
  WHEN site = 'R0A07' THEN 'Yes'
   ELSE 'No'  		
  END AS included_sites

    , CASE 
   WHEN SITE = 'RYJ02' THEN 'Yes'
   WHEN site = 'RQM01' THEN 'Yes'
   WHEN site = 'RJ611' THEN 'Yes' 
   WHEN site = 'R1K04' THEN 'Yes' 
   WHEN site = 'RAS01' THEN 'Yes'
   WHEN site = 'RQXM1' THEN 'Yes' 
   WHEN site = 'RJZ01' THEN 'Yes'
   WHEN site = 'RAX01' THEN 'Yes' 
   WHEN site = 'R0A02' THEN 'Yes'
   WHEN site = 'R1HNH' THEN 'Yes'
   WHEN site = 'R0A66' THEN 'Yes'
   WHEN site = 'RJZ30' THEN 'Yes'
   WHEN site = 'E0A3H' THEN 'Yes' 
   WHEN site = 'RJ701' THEN 'Yes'
   WHEN site = 'RYJ01' THEN 'Yes'
   WHEN site = 'RJ122' THEN 'Yes'
   WHEN site = 'R1H12' THEN 'Yes'
   WHEN site = 'RQM91' THEN 'Yes'
   WHEN site = 'R1HKH' THEN 'Yes'
   WHEN site = 'R0A07' THEN 'Yes' 
      ELSE 'No'  		
  END AS Sentinel_sites

      , CASE 
	WHEN SITE = 'RAL26' THEN 'Yes'
	WHEN site = 'RYJ02' THEN 'Yes'
	WHEN site = 'RQM01' THEN 'Yes'
	WHEN site = 'RJ611' THEN 'Yes'
	WHEN site = 'R1K04' THEN 'Yes'
	WHEN site = 'RAS01' THEN 'Yes'
	WHEN site = 'RQXM1' THEN 'Yes'
    WHEN site = 'RF4DG' THEN 'Yes'
	WHEN site = 'RJZ01' THEN 'Yes'
	WHEN site = 'RAX01' THEN 'Yes'
	WHEN site = 'R1HNH' THEN 'Yes'
	When site = 'RAPNM' THEN 'Yes'
	WHEN site = 'R1K01' THEN 'Yes'
	WHEN site = 'RJZ30' THEN 'Yes'
	WHEN site = 'RJ231' THEN 'Yes'
	WHEN site = 'RF4QH' THEN 'Yes'
	WHEN site = 'RAL01' THEN 'Yes'
	WHEN site = 'RJ701' THEN 'Yes'
	WHEN site = 'RYJ01' THEN 'Yes'
	WHEN site = 'RJ122' THEN 'Yes'
	WHEN site = 'R1H12' THEN 'Yes'
	WHEN site = 'RKEQ4' THEN 'Yes'
	WHEN site = 'RRV03' THEN 'Yes'
    WHEN site = 'RJ224' THEN 'Yes'
	WHEN site = 'RQM91' THEN 'Yes'
	WHEN site = 'R1HKH' THEN 'Yes' 
	WHEN site = 'RVR05' THEN 'Yes'
	      ELSE 'No'  		
  END AS London_sites

     , CASE 
  WHEN SITE = 'RXL01' THEN 'Yes'
  WHEN site = 'RVR50' THEN 'Yes'
  WHEN site = 'R0A02' THEN 'Yes'
  WHEN site = 'R0A66' THEN 'Yes'
  WHEN site = 'E0A3H' THEN 'Yes'
  WHEN site = 'R0A07' THEN 'Yes'
  ELSE 'No'  		
  END AS outside_sites

   , CASE 
   WHEN SITE ='RQXM1' THEN 'Yes'
   WHEN site = 'R1HNH' THEN 'Yes'
   WHEN site = 'RJ122' THEN 'Yes'
   WHEN site = 'R1H12' THEN 'Yes'
   WHEN site = 'R1HKH' THEN 'Yes'
    ELSE 'No'  		
  END AS five_included_site

  , CASE
  WHEN SITE = 'RAL01' and ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
  WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
  WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-03' THEN 'Live' 
  WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN 'Live' 
  WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN 'Live' 
  WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-11-28' THEN 'Live' 
  WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-11-28' THEN 'Live' 
  WHEN SITE = 'RQXM1' and   ARRIVAL_DATE >= '2022-09-12' THEN 'Live' 
  WHEN SITE = 'RQM01' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'RQM91' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'RYJ01' and   ARRIVAL_DATE >= '2022-08-15' THEN 'Live'
  WHEN SITE = 'RYJ02' and   ARRIVAL_DATE >= '2022-07-01' THEN 'Live'  
  WHEN SITE = 'R1K01' and   ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
  WHEN SITE = 'R1K04' and   ARRIVAL_DATE >= '2022-05-19' THEN 'Live'  
  WHEN SITE = 'RAS01' and   ARRIVAL_DATE >= '2022-07-22' THEN 'Live'  
  WHEN SITE = 'RJZ01' and   ARRIVAL_DATE >= '2022-11-16' THEN 'Live'  
  WHEN SITE = 'RJ224' and   ARRIVAL_DATE >= '2023-05-09' THEN 'Live' 
  WHEN SITE = 'RJ231' and   ARRIVAL_DATE >= '2023-05-09' THEN 'Live'  
  WHEN SITE = 'RJ122' and   ARRIVAL_DATE >= '2022-11-01' THEN 'Live' 
  WHEN SITE = 'RJ611' and   ARRIVAL_DATE >= '2023-03-20' THEN 'Live' 
  WHEN SITE = 'RJ701' and   ARRIVAL_DATE >= '2022-11-17' THEN 'Live'  
  WHEN SITE = 'RVR05' and   ARRIVAL_DATE >= '2023-03-07' THEN 'Live' 
  WHEN SITE = 'RVR50' and   ARRIVAL_DATE >= '2023-03-07' THEN 'Live' 
  WHEN SITE = 'RAX01' and   ARRIVAL_DATE >= '2023-04-24' THEN 'Live'  
  WHEN SITE = 'E0A3H' and   ARRIVAL_DATE >= '2023-03-06' THEN 'Live'  
  WHEN SITE = 'R0A66' and   ARRIVAL_DATE >= '2022-09-08' THEN 'Live'  
  WHEN SITE = 'R0A02' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'R0A07' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live'  
  WHEN SITE = 'RXL01' and   ARRIVAL_DATE >= '2023-08-02' THEN 'Live' 
  ELSE 'Not Live'
    END AS live_HCV

	, CASE
	WHEN SITE =  'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live'
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN 'Live' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN 'Live' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN 'Live' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN 'Live' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN 'Live' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN 'Live' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN 'Live' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN 'Live' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN 'Live' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN 'Live' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
					    ELSE 'Not Live'
    END AS live_HBV

	,CASE
	WHEN SITE = 'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN  'Live' 
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN 'Live' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN 'Live' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN 'Live' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN 'Live' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN 'Live' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN 'Live' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN 'Live' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN 'Live' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN 'Live' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN 'Live' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
					    ELSE 'Not Live'
    END AS live_HIV

	
	 , CASE
  WHEN SITE = 'RAL01' and ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22'
  WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22' 
  WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-03' THEN '2023-04-03' 
  WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN '2022-09-05' 
  WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN '2022-04-04' 
  WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-11-28' THEN '2022-11-28' 
  WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-11-28' THEN '2022-11-28' 
  WHEN SITE = 'RQXM1' and   ARRIVAL_DATE >= '2022-09-12' THEN '2022-09-12' 
  WHEN SITE = 'RQM01' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'RQM91' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'RYJ01' and   ARRIVAL_DATE >= '2022-08-15' THEN '2022-08-15'
  WHEN SITE = 'RYJ02' and   ARRIVAL_DATE >= '2022-07-01' THEN '2022-07-01'  
  WHEN SITE = 'R1K01' and   ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
  WHEN SITE = 'R1K04' and   ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19'  
  WHEN SITE = 'RAS01' and   ARRIVAL_DATE >= '2022-07-22' THEN '2022-07-22'  
  WHEN SITE = 'RJZ01' and   ARRIVAL_DATE >= '2022-11-16' THEN '2022-11-16'  
  WHEN SITE = 'RJ224' and   ARRIVAL_DATE >= '2023-05-09' THEN '2023-05-09' 
  WHEN SITE = 'RJ231' and   ARRIVAL_DATE >= '2023-05-09' THEN '2023-05-09'  
  WHEN SITE = 'RJ122' and   ARRIVAL_DATE >= '2022-11-01' THEN '2022-11-01' 
  WHEN SITE = 'RJ611' and   ARRIVAL_DATE >= '2023-03-20' THEN '2023-03-20' 
  WHEN SITE = 'RJ701' and   ARRIVAL_DATE >= '2022-11-17' THEN '2022-11-17'  
  WHEN SITE = 'RVR05' and   ARRIVAL_DATE >= '2023-03-07' THEN '2023-03-07' 
  WHEN SITE = 'RVR50' and   ARRIVAL_DATE >= '2023-03-07' THEN '2023-03-07' 
  WHEN SITE = 'RAX01' and   ARRIVAL_DATE >= '2023-04-24' THEN '2023-04-24'  
  WHEN SITE = 'E0A3H' and   ARRIVAL_DATE >= '2023-03-06' THEN '2023-03-06'  
  WHEN SITE = 'R0A66' and   ARRIVAL_DATE >= '2022-09-08' THEN '2022-09-08'  
  WHEN SITE = 'R0A02' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'R0A07' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01'  
  WHEN SITE = 'RXL01' and   ARRIVAL_DATE >= '2023-08-02' THEN '2023-08-02' 
  ELSE null
    END AS live_HCV_Date

	, CASE
	WHEN SITE =  'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22'
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN '2023-04-01' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN '2022-09-05' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN '2022-04-04' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN '2022-08-01' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN '2022-07-28' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN '2022-07-22' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN '2022-04-21' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN '2022-05-16' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN '2022-04-06' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN '2022-09-08' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
					    ELSE null
    END AS live_HBV_Date

	,CASE
	WHEN SITE = 'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN  '2022-04-22' 
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN '2022-09-05' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN '2022-04-04' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN '2022-08-01' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN '2022-07-28' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN '2022-07-22' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN '2022-04-21' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN '2022-05-16' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN '2022-04-06' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN '2022-09-08' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
					    ELSE NULL
    END AS live_HIV_Date

	, CASE
	WHEN INVESTIGATION_CODE_Description = 'Serology' THEN 'bloods_any'
	WHEN INVESTIGATION_CODE_Description = 'Urea & Electrolytes (U&Es)' THEN 'bloods_any'
	WHEN INVESTIGATION_CODE_Description = 'Full blood count (FBC)' THEN 'bloods_any'
	WHEN INVESTIGATION_CODE_Description = 'Liver function tests (LFTs)' THEN 'bloods_any'
						    ELSE null
    END AS ECDS_bloods_any

  INTO [12monthECDS]
  FROM [ECDS].[dbo].[vtECDS]
  LEFT JOIN [ECDS].[dbo].[vtECDS_INVESTIGATION]
  ON [ECDS].[dbo].[vtECDS].[PheKey] = [ECDS].[dbo].[vtECDS_INVESTIGATION].[PheKey] AND [ECDS].[dbo].[vtECDS].[FYEAR] = [ECDS].[dbo].[vtECDS_INVESTIGATION].[FYEAR]
  LEFT JOIN [Y006_BBV_PID].[dbo].[vECDS_PID]
  ON [ECDS].[dbo].[vtECDS].[PheKey] = [Y006_BBV_PID].[dbo].[vECDS_PID].[rowid] AND [ECDS].[dbo].[vtECDS].[FYEAR] = [Y006_BBV_PID].[dbo].[vECDS_PID].[FYEAR]
  LEFT JOIN [LookupsShared].[dbo].[vSocioDemog_LSOA11]
   ON [ECDS].[dbo].[vtECDS].[LSOA_2011] = [LookupsShared].[dbo].[vSocioDemog_LSOA11].[LSOA11CD]
  WHERE [PROVIDER_CODE_DERIVED] in ('RAL', 'RAP', 'RRV', 'RKE', 'R1H', 'RF4', 'RQX', 'RQM', 'RYJ', 'R1K', 'RAS', 'RJZ', 'RJ2',
'RJ1', 'RJ6', 'RJ7', 'RVR', 'RAX', 'RTP', 'R0A', 'RM3', 'RXL', 'RYR') -- include down by live sites
  and [AGE_AT_ARRIVAL] > 15
  AND [ECDS].[dbo].[vtECDS].[FYEAR] = '2223'
  and [PROVIDER_CODE_DERIVED] in ('RAL', 'RAP', 'RRV', 'RKE', 'R1H', 'RF4', 'RQX', 'RQM', 'RYJ', 'R1K', 'RAS', 'RJZ', 'RJ2',
'RJ1', 'RJ6', 'RJ7', 'RVR', 'RAX', 'RTP', 'R0A', 'RM3', 'RXL', 'RYR')
GROUP BY  [vtECDS].PheKey
  ,TOKEN_PERSON_ID
  ,[Y006_BBV_PID].[dbo].[vECDS_PID].[BIRTH_DATE]
  , [Y006_BBV_PID].[dbo].[vECDS_PID].[NHS_NUMBER]
  ,[Y006_BBV_PID].[dbo].[vECDS_PID].[POSTCODE]
  ,[PROVIDER_CODE_DERIVED]
  ,[SITE]
  ,[ARRIVAL_MONTH]
  ,[ARRIVAL_DATE]
  ,[ECDS].[dbo].[vtECDS].[FYEAR]
  ,[InvestigationIdx]
  ,[INVESTIGATION_DATE]
  ,[INVESTIGATION_CODE_Description]
  ,[AGE_AT_ARRIVAL]
    , CASE 
  WHEN [AGE_AT_ARRIVAL] between 0 and 15 THEN 'Under 16' 
  WHEN [AGE_AT_ARRIVAL] between 16 and 24 THEN '16-24' 
  WHEN [AGE_AT_ARRIVAL] between 25 and 34 THEN '25-34' 
  WHEN [AGE_AT_ARRIVAL] between 35 and 49 THEN '35-49' 
  WHEN [AGE_AT_ARRIVAL] between 50 and 64 THEN '50-64' 
  WHEN [AGE_AT_ARRIVAL] between 65 and 79 THEN '65-79' 
  WHEN [AGE_AT_ARRIVAL] between 80 and 130 THEN '80+' 
  ELSE null		
  END 
      , CASE 
  WHEN [AGE_AT_ARRIVAL] between 0 and 15 THEN 'Under 16' 
  WHEN [AGE_AT_ARRIVAL] between 16 and 44 THEN '16-44' 
  WHEN [AGE_AT_ARRIVAL] between 45 and 64 THEN '45-64' 
  WHEN [AGE_AT_ARRIVAL] between 65 and 130 THEN '65+' 
  ELSE null		
  END 

  , case
    WHEN [ETHNIC_CATEGORY_Description] = 'White British' THEN 'White British' 
	    WHEN [ETHNIC_CATEGORY_Description] = 'Any other White background' THEN 'White Other' 
		    WHEN [ETHNIC_CATEGORY_Description] = 'White Irish' THEN 'White Other' 
			    WHEN [ETHNIC_CATEGORY_Description] = 'African' THEN 'Black African' 
				    WHEN [ETHNIC_CATEGORY_Description] = 'Caribbean' THEN 'Black Caribbean' 
				    WHEN [ETHNIC_CATEGORY_Description] = 'Any other Black background' THEN 'Black Other' 
					 WHEN [ETHNIC_CATEGORY_Description] = 'Indian' THEN 'Indian, Pakistani or Bangladeshi' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Bangladeshi' THEN 'Indian, Pakistani or Bangladeshi' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Pakistani' THEN 'Indian, Pakistani or Bangladeshi' 
				    WHEN [ETHNIC_CATEGORY_Description] = 'Chinese' THEN 'Asian Other' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Any other Asian background' THEN 'Asian Other' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Any other mixed background' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'White and Asian' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'White and Black African' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'White and Black Caribbean' THEN 'Mixed/Multiple' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Any other ethnic group' THEN 'Other' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Not known e.g. unconscious' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = '<NA>' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = 'NA' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = '' THEN 'Unknown' 
					WHEN [ETHNIC_CATEGORY_Description] = 'Not stated e.g. unwilling to state' THEN 'Unknown' 
    ELSE null		
  END 
             
  ,[LSOA_2011]
  ,[LookupsShared].[dbo].[vSocioDemog_LSOA11].[IMD2019_Deciles_LSOA11_withinUTLA21]
  ,[ETHNIC_CATEGORY]
  ,[ETHNIC_CATEGORY_Description]
   ,[STATED_GENDER]
  ,[INTERPRETER_LANGUAGE_Description]
  ,[INTERPRETER_LANGUAGE_VALID]
  ,[PREFERRED_SPOKEN_LANGUAGE_Description]
  ,[PREFERRED_SPOKEN_LANGUAGE_VALID]
  ,[ACCOMMODATION_STATUS_Description]
  ,[ACCOMMODATION_STATUS_VALID]
  ,[DRUG_ALCOHOL_CODE_1_Description]


  , CASE 			
  WHEN [PROVIDER_CODE_DERIVED] in ('RRV','RP4','RAL','RAN','RAP','RRP','RP6','RKE','RNK') THEN 'NCL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('R1H','RF4','RWK','RQX','RAT')   THEN 'NEL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('RYJ','RT3','RQM','RKL','R1K','RV3','RAS','RYX') THEN 'NWL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ1','RJZ','RV5','RJ2','RPG')   THEN 'SEL'		
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ7','RVR','RQY','RJ6','RAX','RPY')   THEN 'SWL'	
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ7','RVR','RQY','RJ6','RAX','RPY')   THEN 'SE'	
  ELSE 'Other'  		
  END 
  
  , CASE 
  WHEN [PROVIDER_CODE_DERIVED] in ('RTP') THEN 'UNIVERSITY HOSPITALS SUSSEX NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('R0A') THEN 'MANCHESTER UNIVERSITY NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('RM3') THEN 'SALFORD ROYAL NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('RXL') THEN 'BLACKPOOL TEACHING HOSPITALS NHS FOUNDATION TRUST' 
  WHEN [PROVIDER_CODE_DERIVED] in ('RRV') THEN 'UNIVERSITY COLLEGE LONDON HOSPITALS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RP4') THEN 'GREAT ORMOND STREET HOSPITAL FOR CHILDREN NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAL') THEN 'ROYAL FREE LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RP6') THEN 'MOORFIELDS EYE HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAN') THEN 'ROYAL NATIONAL ORTHOPAEDIC HOSPITAL NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAP') THEN 'NORTH MIDDLESEX UNIVERSITY HOSPITAL NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RRP') THEN 'BARNET, ENFIELD AND HARINGEY MENTAL HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RKE') THEN 'THE WHITTINGTON HOSPITAL NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RNK') THEN 'TAVISTOCK AND PORTMAN NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('R1H') THEN 'BARTS HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RF4') THEN 'BARKING, HAVERING AND REDBRIDGE UNIVERSITY HOSPITALS NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RWK') THEN 'EAST LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RQX') THEN 'HOMERTON UNIVERSITY HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAT') THEN 'NORTH EAST LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RYJ') THEN 'IMPERIAL COLLEGE HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RT3') THEN 'ROYAL BROMPTON & HAREFIELD NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RQM') THEN 'CHELSEA AND WESTMINSTER HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RKL') THEN 'WEST LONDON MENTAL HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RV3') THEN 'CENTRAL AND NORTH WEST LONDON NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAS') THEN 'THE HILLINGDON HOSPITALS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RYX') THEN 'CENTRAL LONDON COMMUNITY HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ1') THEN 'GUYS AND ST THOMAS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJZ') THEN 'KINGS COLLEGE HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RV5') THEN 'SOUTH LONDON AND MAUDSLEY NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ2') THEN 'LEWISHAM HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RPG') THEN 'OXLEAS NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ7') THEN 'ST GEORGES HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RVR') THEN 'EPSOM AND ST HELIER UNIVERSITY HOSPITALS NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RQY') THEN 'SOUTH WEST LONDON AND ST GEORGES MENTAL HEALTH NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RJ6') THEN 'CROYDON HEALTH SERVICES NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RAX') THEN 'KINGSTON HOSPITAL NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RPY') THEN 'THE ROYAL MARSDEN NHS FOUNDATION TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('R1K') THEN 'LONDON NORTH WEST UNIVERSITY HEALTHCARE NHS TRUST'
  WHEN [PROVIDER_CODE_DERIVED] in ('RYR') THEN 'University Hospitals Sussex NHS Foundation Trust'
  ELSE 'Other'  		
  END 

   , CASE 
  WHEN SITE = 'RAL26' THEN 'Yes'
  WHEN Site = 'RXL01' THEN 'Yes'
  WHEN site = 'RYJ02' THEN 'Yes'
  WHEN site = 'RQM01' THEN 'Yes'
  WHEN site = 'RJ611' THEN 'Yes'
  WHEN site = 'R1K04' THEN 'Yes'
  WHEN site = 'RVR50' THEN 'Yes'
  WHEN site = 'RAS01' THEN 'Yes'
  WHEN site = 'RQXM1' THEN 'Yes'
  WHEN site = 'RF4DG' THEN 'Yes'
  WHEN site = 'RJZ01' THEN 'Yes'
  WHEN site = 'RAX01' THEN 'Yes'
  WHEN site = 'R0A02' THEN 'Yes'
  WHEN site = 'R1HNH' THEN 'Yes'
  WHEN site = 'R0A66' THEN 'Yes'
  WHEN site = 'RAPNM' THEN 'Yes'
  WHEN site = 'R1K01' THEN 'Yes'
  WHEN site = 'RJZ30' THEN 'Yes'
  WHEN site = 'RJ231' THEN 'Yes'
  WHEN site = 'RF4QH' THEN 'Yes'
  WHEN site = 'RAL01' THEN 'Yes'
  WHEN site = 'E0A3H' THEN 'Yes'
  WHEN site = 'RJ701' THEN 'Yes'
  WHEN site = 'RVR05' THEN 'Yes'
  WHEN site = 'RYJ01' THEN 'Yes'
  WHEN site = 'RJ122' THEN 'Yes'
  WHEN site = 'R1H12' THEN 'Yes'
  WHEN site = 'RKEQ4' THEN 'Yes' 
  WHEN site = 'RRV03' THEN 'Yes'
  WHEN site = 'RJ224' THEN 'Yes'
  WHEN site = 'RQM91' THEN 'Yes'
  WHEN site = 'R1HKH' THEN 'Yes'
  WHEN site = 'R0A07' THEN 'Yes'
   ELSE 'No'  		
  END

    , CASE 
   WHEN SITE = 'RYJ02' THEN 'Yes'
   WHEN site = 'RQM01' THEN 'Yes'
   WHEN site = 'RJ611' THEN 'Yes' 
   WHEN site = 'R1K04' THEN 'Yes' 
   WHEN site = 'RAS01' THEN 'Yes'
   WHEN site = 'RQXM1' THEN 'Yes' 
   WHEN site = 'RJZ01' THEN 'Yes'
   WHEN site = 'RAX01' THEN 'Yes' 
   WHEN site = 'R0A02' THEN 'Yes'
   WHEN site = 'R1HNH' THEN 'Yes'
   WHEN site = 'R0A66' THEN 'Yes'
   WHEN site = 'RJZ30' THEN 'Yes'
   WHEN site = 'E0A3H' THEN 'Yes' 
   WHEN site = 'RJ701' THEN 'Yes'
   WHEN site = 'RYJ01' THEN 'Yes'
   WHEN site = 'RJ122' THEN 'Yes'
   WHEN site = 'R1H12' THEN 'Yes'
   WHEN site = 'RQM91' THEN 'Yes'
   WHEN site = 'R1HKH' THEN 'Yes'
   WHEN site = 'R0A07' THEN 'Yes' 
      ELSE 'No'  		
  END

      , CASE 
	WHEN SITE = 'RAL26' THEN 'Yes'
	WHEN site = 'RYJ02' THEN 'Yes'
	WHEN site = 'RQM01' THEN 'Yes'
	WHEN site = 'RJ611' THEN 'Yes'
	WHEN site = 'R1K04' THEN 'Yes'
	WHEN site = 'RAS01' THEN 'Yes'
	WHEN site = 'RQXM1' THEN 'Yes'
    WHEN site = 'RF4DG' THEN 'Yes'
	WHEN site = 'RJZ01' THEN 'Yes'
	WHEN site = 'RAX01' THEN 'Yes'
	WHEN site = 'R1HNH' THEN 'Yes'
	When site = 'RAPNM' THEN 'Yes'
	WHEN site = 'R1K01' THEN 'Yes'
	WHEN site = 'RJZ30' THEN 'Yes'
	WHEN site = 'RJ231' THEN 'Yes'
	WHEN site = 'RF4QH' THEN 'Yes'
	WHEN site = 'RAL01' THEN 'Yes'
	WHEN site = 'RJ701' THEN 'Yes'
	WHEN site = 'RYJ01' THEN 'Yes'
	WHEN site = 'RJ122' THEN 'Yes'
	WHEN site = 'R1H12' THEN 'Yes'
	WHEN site = 'RKEQ4' THEN 'Yes'
	WHEN site = 'RRV03' THEN 'Yes'
    WHEN site = 'RJ224' THEN 'Yes'
	WHEN site = 'RQM91' THEN 'Yes'
	WHEN site = 'R1HKH' THEN 'Yes' 
	WHEN site = 'RVR05' THEN 'Yes'
	      ELSE 'No'  		
  END

     , CASE 
  WHEN SITE = 'RXL01' THEN 'Yes'
  WHEN site = 'RVR50' THEN 'Yes'
  WHEN site = 'R0A02' THEN 'Yes'
  WHEN site = 'R0A66' THEN 'Yes'
  WHEN site = 'E0A3H' THEN 'Yes'
  WHEN site = 'R0A07' THEN 'Yes'
  ELSE 'No'  		
  END

   , CASE 
   WHEN SITE ='RQXM1' THEN 'Yes'
   WHEN site = 'R1HNH' THEN 'Yes'
   WHEN site = 'RJ122' THEN 'Yes'
   WHEN site = 'R1H12' THEN 'Yes'
   WHEN site = 'R1HKH' THEN 'Yes'
    ELSE 'No'  		
  END

  , CASE
  WHEN SITE = 'RAL01' and ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
  WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
  WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-03' THEN 'Live' 
  WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN 'Live' 
  WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN 'Live' 
  WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-11-28' THEN 'Live' 
  WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-11-28' THEN 'Live' 
  WHEN SITE = 'RQXM1' and   ARRIVAL_DATE >= '2022-09-12' THEN 'Live' 
  WHEN SITE = 'RQM01' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'RQM91' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'RYJ01' and   ARRIVAL_DATE >= '2022-08-15' THEN 'Live'
  WHEN SITE = 'RYJ02' and   ARRIVAL_DATE >= '2022-07-01' THEN 'Live'  
  WHEN SITE = 'R1K01' and   ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
  WHEN SITE = 'R1K04' and   ARRIVAL_DATE >= '2022-05-19' THEN 'Live'  
  WHEN SITE = 'RAS01' and   ARRIVAL_DATE >= '2022-07-22' THEN 'Live'  
  WHEN SITE = 'RJZ01' and   ARRIVAL_DATE >= '2022-11-16' THEN 'Live'  
  WHEN SITE = 'RJ224' and   ARRIVAL_DATE >= '2023-05-09' THEN 'Live' 
  WHEN SITE = 'RJ231' and   ARRIVAL_DATE >= '2023-05-09' THEN 'Live'  
  WHEN SITE = 'RJ122' and   ARRIVAL_DATE >= '2022-11-01' THEN 'Live' 
  WHEN SITE = 'RJ611' and   ARRIVAL_DATE >= '2023-03-20' THEN 'Live' 
  WHEN SITE = 'RJ701' and   ARRIVAL_DATE >= '2022-11-17' THEN 'Live'  
  WHEN SITE = 'RVR05' and   ARRIVAL_DATE >= '2023-03-07' THEN 'Live' 
  WHEN SITE = 'RVR50' and   ARRIVAL_DATE >= '2023-03-07' THEN 'Live' 
  WHEN SITE = 'RAX01' and   ARRIVAL_DATE >= '2023-04-24' THEN 'Live'  
  WHEN SITE = 'E0A3H' and   ARRIVAL_DATE >= '2023-03-06' THEN 'Live'  
  WHEN SITE = 'R0A66' and   ARRIVAL_DATE >= '2022-09-08' THEN 'Live'  
  WHEN SITE = 'R0A02' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
  WHEN SITE = 'R0A07' and   ARRIVAL_DATE >= '2022-04-01' THEN 'Live'  
  WHEN SITE = 'RXL01' and   ARRIVAL_DATE >= '2023-08-02' THEN 'Live' 
  ELSE 'Not Live'
    END

	, CASE
	WHEN SITE =  'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live'
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN 'Live' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN 'Live' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN 'Live' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN 'Live' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN 'Live' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN 'Live' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN 'Live' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN 'Live' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN 'Live' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN 'Live' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
					    ELSE 'Not Live'
    END

	,CASE
	WHEN SITE = 'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN  'Live' 
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN 'Live' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN 'Live' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN 'Live' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN 'Live' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN 'Live' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN 'Live' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN 'Live' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN 'Live' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN 'Live' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN 'Live' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN 'Live' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN 'Live' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN 'Live' 
					    ELSE 'Not Live'
    END

	 , CASE
  WHEN SITE = 'RAL01' and ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22'
  WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22' 
  WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-03' THEN '2023-04-03' 
  WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN '2022-09-05' 
  WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN '2022-04-04' 
  WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-11-28' THEN '2022-11-28' 
  WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-11-28' THEN '2022-11-28' 
  WHEN SITE = 'RQXM1' and   ARRIVAL_DATE >= '2022-09-12' THEN '2022-09-12' 
  WHEN SITE = 'RQM01' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'RQM91' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'RYJ01' and   ARRIVAL_DATE >= '2022-08-15' THEN '2022-08-15'
  WHEN SITE = 'RYJ02' and   ARRIVAL_DATE >= '2022-07-01' THEN '2022-07-01'  
  WHEN SITE = 'R1K01' and   ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
  WHEN SITE = 'R1K04' and   ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19'  
  WHEN SITE = 'RAS01' and   ARRIVAL_DATE >= '2022-07-22' THEN '2022-07-22'  
  WHEN SITE = 'RJZ01' and   ARRIVAL_DATE >= '2022-11-16' THEN '2022-11-16'  
  WHEN SITE = 'RJ224' and   ARRIVAL_DATE >= '2023-05-09' THEN '2023-05-09' 
  WHEN SITE = 'RJ231' and   ARRIVAL_DATE >= '2023-05-09' THEN '2023-05-09'  
  WHEN SITE = 'RJ122' and   ARRIVAL_DATE >= '2022-11-01' THEN '2022-11-01' 
  WHEN SITE = 'RJ611' and   ARRIVAL_DATE >= '2023-03-20' THEN '2023-03-20' 
  WHEN SITE = 'RJ701' and   ARRIVAL_DATE >= '2022-11-17' THEN '2022-11-17'  
  WHEN SITE = 'RVR05' and   ARRIVAL_DATE >= '2023-03-07' THEN '2023-03-07' 
  WHEN SITE = 'RVR50' and   ARRIVAL_DATE >= '2023-03-07' THEN '2023-03-07' 
  WHEN SITE = 'RAX01' and   ARRIVAL_DATE >= '2023-04-24' THEN '2023-04-24'  
  WHEN SITE = 'E0A3H' and   ARRIVAL_DATE >= '2023-03-06' THEN '2023-03-06'  
  WHEN SITE = 'R0A66' and   ARRIVAL_DATE >= '2022-09-08' THEN '2022-09-08'  
  WHEN SITE = 'R0A02' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
  WHEN SITE = 'R0A07' and   ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01'  
  WHEN SITE = 'RXL01' and   ARRIVAL_DATE >= '2023-08-02' THEN '2023-08-02' 
  ELSE null
    END

	, CASE
	WHEN SITE =  'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22'
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN '2023-04-01' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN '2022-09-05' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN '2022-04-04' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN '2022-08-01' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN '2022-07-28' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN '2022-07-22' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN '2022-04-21' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN '2022-05-16' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN '2022-04-06' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN '2022-09-08' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
					    ELSE null
    END

	,CASE
	WHEN SITE = 'RAL01'  and  ARRIVAL_DATE >= '2022-04-22' THEN  '2022-04-22' 
                      WHEN SITE = 'RAL26' and  ARRIVAL_DATE >= '2022-04-22' THEN '2022-04-22' 
                      WHEN SITE = 'RAPNM' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RRV03' and  ARRIVAL_DATE >= '2023-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RKEQ4' and  ARRIVAL_DATE >= '2022-09-05' THEN '2022-09-05' 
                      WHEN SITE = 'R1HNH' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1H12' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1HKH' and  ARRIVAL_DATE >= '2022-04-04' THEN '2022-04-04' 
                      WHEN SITE = 'RF4QH' and  ARRIVAL_DATE >= '2022-08-01' THEN '2022-08-01' 
                      WHEN SITE = 'RF4DG' and  ARRIVAL_DATE >= '2022-07-28' THEN '2022-07-28' 
                      WHEN SITE = 'RQXM1' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RQM91' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RYJ02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R1K01' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'R1K04' and  ARRIVAL_DATE >= '2022-05-19' THEN '2022-05-19' 
                      WHEN SITE = 'RAS01' and  ARRIVAL_DATE >= '2022-07-22' THEN '2022-07-22' 
                      WHEN SITE = 'RJZ01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJZ30' and  ARRIVAL_DATE >= '2022-04-21' THEN '2022-04-21' 
                      WHEN SITE = 'RJ224' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ231' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ122' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ611' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RJ701' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR05' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RVR50' and  ARRIVAL_DATE >= '2022-05-16' THEN '2022-05-16' 
                      WHEN SITE = 'RAX01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'E0A3H' and  ARRIVAL_DATE >= '2022-04-06' THEN '2022-04-06' 
                      WHEN SITE = 'R0A66' and  ARRIVAL_DATE >= '2022-09-08' THEN '2022-09-08' 
                      WHEN SITE = 'R0A02' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'R0A07' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
                      WHEN SITE = 'RXL01' and  ARRIVAL_DATE >= '2022-04-01' THEN '2022-04-01' 
					    ELSE NULL
    END
		, CASE
	WHEN INVESTIGATION_CODE_Description = 'Serology' THEN 'bloods_any'
	WHEN INVESTIGATION_CODE_Description = 'Urea & Electrolytes (U&Es)' THEN 'bloods_any'
	WHEN INVESTIGATION_CODE_Description = 'Full blood count (FBC)' THEN 'bloods_any'
	WHEN INVESTIGATION_CODE_Description = 'Liver function tests (LFTs)' THEN 'bloods_any'
						    ELSE null
    END ;

	ALTER TABLE [12monthECDS]
	ADD BBVtest VARCHAR (255)
, HCV VARCHAR (255)
, HBV VARCHAR (255)
, HIV VARCHAR (255)
, HCVdiff VARCHAR (255)
, HBVdiff VARCHAR (255)
, HIVdiff VARCHAR (255)
, HCVresult VARCHAR (255)
, HBVresult VARCHAR (255)
, HIVresult VARCHAR (255)
, HCVAb VARCHAR (255)
, HCVPCR VARCHAR (255)
, HBVAg VARCHAR (255)
, HBVVL VARCHAR (255);



--HCV test
UPDATE [12monthECDS]
SET HCV = 'Yes'
, HCVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HCV_Antibody] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCV is null and [live_HCV] = 'Live' ;

UPDATE [12monthECDS]
SET HCV = 'Yes'
, HCVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [HCV_Antibody] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7 and [12monthECDS].HCV is null and  [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCV = 'Yes'
, HCVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HCV_Antigen] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCV is null and [live_HCV] = 'Live'  ;

UPDATE [12monthECDS]
SET HCV = 'Yes'
, HCVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [HCV_Antigen] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7 and [12monthECDS].HCV is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCV = 'Yes'
, HCVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [EmergencyDepartment_tests].[HCVPCR] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCV is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCV = 'Yes'
, HCVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [EmergencyDepartment_tests].[HCVPCR] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7 and [12monthECDS].HCV is null and [live_HCV] = 'Live';

--HCV result (Positive / Negative / indeterminate / equivocal)

UPDATE [12monthECDS]
SET HCVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [ExcludeAb] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVresult is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [ExcludeAb] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVresult is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [ExcludeAg] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVresult is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [ExcludeAg] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVresult is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [ExcludePCR] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVresult is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [ExcludePCR] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVresult is null and [live_HCV] = 'Live';

--HCV pos/neg

UPDATE [12monthECDS]
SET HCVAb = 'Positive'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HCV_Antibody] = 'positive' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVAb is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVAb = 'Negative'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HCV_Antibody] = 'Negative' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVAb is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVPCR = 'Positive'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [EmergencyDepartment_tests].[HCVPCR] = 'positive' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVPCR is null and [live_HCV] = 'Live';

UPDATE [12monthECDS]
SET HCVPCR = 'Negative'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [EmergencyDepartment_tests].[HCVPCR] = 'Negative' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HCVPCR is null and [live_HCV] = 'Live';


--HBV

UPDATE [12monthECDS]
SET HBV = 'Yes'
, HBVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HBVSurfaceAntigen] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HBV is null and [live_HBV] = 'Live';

UPDATE [12monthECDS]
SET HBV = 'Yes'
, HBVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [HBVSurfaceAntigen] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7 and [12monthECDS].HBV is null and [live_HBV] = 'Live';

UPDATE [12monthECDS]
SET HBV = 'Yes'
, HBVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HBVVIRALLOAD] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HBV is null  and [live_HBV] = 'Live';

UPDATE [12monthECDS]
SET HBV = 'Yes'
, HBVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [HBVVIRALLOAD] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7 and [12monthECDS].HBV is null  and [live_HBV] = 'Live';

--HBV result (Positive / Negative / indeterminate / equivocal)

UPDATE [12monthECDS]
SET HBVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [ExcludeHBVAg] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HBVresult is null  and [live_HBV] = 'Live';

UPDATE [12monthECDS]
SET HBVresult = 'Yes'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [ExcludeHBVVL] = 'result' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HBVresult is null  and [live_HBV] = 'Live';

--HCV pos/neg

UPDATE [12monthECDS]
SET [HBVAg] = 'Positive'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HBVSurfaceAntigen] = 'positive' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].[HBVAg] is null  and [live_HBV] = 'Live';

UPDATE [12monthECDS]
SET [HBVAg] = 'Negative'
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [HBVSurfaceAntigen] = 'Negative' and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].[HBVAg] is null  and [live_HBV] = 'Live';

UPDATE [12monthECDS]
SET [HBVVL] = [HBVVIRALLOAD]
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [EmergencyDepartment_tests].[HBVVIRALLOAD] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].[HBVVL] is null  and [live_HBV] = 'Live';

SELECT [HBVVL]
FROM [12monthECDS]
GROUP BY [HBVVL];

UPDATE [12monthECDS]
SET [HBVVL] = null
WHERE [HBVVL] = 'Test not required' or [HBVVL] = ';A PINK EDTA SAMPLE IS REQUIRED.' or [HBVVL] = 'Insufficient specimen for test' or [HBVVL] = 'Duplicate request' or [HBVVL] = 'not tested'


--HIV

UPDATE [12monthECDS]
SET HIV = 'Yes'
, HIVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [EmergencyDepartment_tests].[HIV] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7  and [12monthECDS].HIV is null  and [live_HIV] = 'Live';

UPDATE [12monthECDS]
SET HIV = 'Yes'
, HIVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [EmergencyDepartment_tests].[HIV] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7 and [12monthECDS].HIV is null  and [live_HIV] = 'Live';

UPDATE [12monthECDS]
SET HIV = 'Yes'
, HIVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[trust] = [dbo].[12monthECDS].[Provider_derived] 
Where [EmergencyDepartment_tests].[HIVVIRALLOAD] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 0  and [12monthECDS].HIV is null and [live_HIV] = 'Live';

UPDATE [12monthECDS]
SET HIV = 'Yes'
, HIVdiff = DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate])
FROM [dbo].[EmergencyDepartment_tests]
INNER JOIN [12monthECDS]
ON [EmergencyDepartment_tests].[PtNHSNumber] = [dbo].[12monthECDS].[NHS_NUMBER] 
AND [EmergencyDepartment_tests].[Providercode] = [dbo].[12monthECDS].[PROVIDER_CODE_DERIVED] 
Where [EmergencyDepartment_tests].[HIVVIRALLOAD] is not null and DATEDIFF(day,[ARRIVAL_DATE],[SpecimenDate]) between 0 and 7 and [12monthECDS].HIV is null and [live_HIV] = 'Live';



DROP TABLE [12monthECDSattendances]

SELECT TOKEN_PERSON_ID, [ARRIVAL_DATE]
INTO [12monthECDSattendancesv2]
FROM [12monthECDS]
GROUP BY TOKEN_PERSON_ID, [ARRIVAL_DATE];

ALTER TABLE [12monthECDSattendancesv2]
ADD [SITE] VARCHAR (255)
, [PROVIDER_CODE_DERIVED] VARCHAR (255)
, [Provider_derived]  VARCHAR (255)
, minarrival VARCHAR (255)
, [ECDS_bloods_any] VARCHAR (255)
, [included_sites] VARCHAR (255)
, [Sentinel_sites] VARCHAR (255)
, [London_sites] VARCHAR (255)
, [outside_sites] VARCHAR (255)
, [five_included_site] VARCHAR (255)
, [Sex] VARCHAR (255)
, [Age] int
, [age_group] VARCHAR (255)
, [age_group2] VARCHAR (255)
, [ethnic_group] VARCHAR (255)
, [IMD] VARCHAR (255)
,[live_HCV]VARCHAR (255)
,[live_HBV]VARCHAR (255)
,[live_HIV]VARCHAR (255)
, [HCV] VARCHAR (255)
, [HBV] VARCHAR (255)
, [HIV] VARCHAR (255)
, [HCVresult] VARCHAR (255)
, [HBVresult] VARCHAR (255)
, [HIVresult] VARCHAR (255)
, [HCVAb] VARCHAR (255)
, [HCVPCR] VARCHAR (255)
, [HBVAg] VARCHAR (255)
, [HBVVL] VARCHAR (255);


UPDATE [12monthECDSattendancesv2]
Set [SITE]  = [12monthECDS].[SITE]
, [PROVIDER_CODE_DERIVED]  = [12monthECDS].[PROVIDER_CODE_DERIVED]
, [Provider_derived]  = [12monthECDS].[Provider_derived]
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE;


UPDATE [12monthECDSattendancesv2]
Set [ECDS_bloods_any] = 'bloods_any'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[ECDS_bloods_any] = 'bloods_any';


UPDATE [12monthECDSattendancesv2]
Set [age] = [12monthECDS].[AGE_AT_ARRIVAL]
, [age_group] = [12monthECDS].[age_group]
, [age_group2] = [12monthECDS].[age_group2]
, [ethnic_group] = [12monthECDS].[ethnic_group]
, [IMD] = [12monthECDS].[IMD2019_Deciles_LSOA11_withinUTLA21]
, [Sex] = [12monthECDS].[STATED_GENDER]
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE;

UPDATE [12monthECDSattendancesv2]
Set [included_sites] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[included_sites] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [Sentinel_sites] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[Sentinel_sites] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [London_sites] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[London_sites] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [outside_sites] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[outside_sites] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [five_included_site] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[five_included_site] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [live_HCV] = 'Live'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[live_HCV] = 'Live';

UPDATE [12monthECDSattendancesv2]
Set [live_HBV] = 'Live'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[live_HBV] = 'Live';

UPDATE [12monthECDSattendancesv2]
Set [live_HIV] = 'Live'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[live_HIV] = 'Live';

UPDATE [12monthECDSattendancesv2]
Set [HCV] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HCV] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [HBV] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HBV] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [HIV] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HIV] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [HCVresult] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HCVresult] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [HBVresult] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HBVresult] = 'Yes';

UPDATE [12monthECDSattendancesv2]
Set [HIVresult] = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HIVresult] = 'Yes';


UPDATE [12monthECDSattendancesv2]
Set [HCVAb] = [12monthECDS].[HCVAb]
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HCVAb] is not null;

UPDATE [12monthECDSattendancesv2]
Set [HCVPCR] = [12monthECDS].[HCVPCR]
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HCVPCR] is not null;

UPDATE [12monthECDSattendancesv2]
Set [HBVAg] = [12monthECDS].[HBVAg]
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HBVAg] is not null;

UPDATE [12monthECDSattendancesv2]
Set [HBVVL] = [12monthECDS].[HBVVL]
FROM [12monthECDSattendancesv2]
INNER JOIN [12monthECDS]
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = [12monthECDS].TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = [12monthECDS].ARRIVAL_DATE
WHERE [12monthECDS].[HBVVL] is not null;



WITH X AS (SELECT TOKEN_PERSON_ID, min([ARRIVAL_DATE]) mindate
FROM [12monthECDSattendancesv2]
Where included_sites = 'Yes' and [ECDS_bloods_any] is not null and ([live_HIV] = 'Live' or [live_HBV] = 'Live' or [live_HCV] = 'Live')
GROUP BY TOKEN_PERSON_ID)
UPDATE [12monthECDSattendancesv2]
SET minarrival = 'Yes'
FROM [12monthECDSattendancesv2]
INNER JOIN X
ON [12monthECDSattendancesv2].TOKEN_PERSON_ID = X.TOKEN_PERSON_ID
AND [12monthECDSattendancesv2].ARRIVAL_DATE = X.mindate;



-- [12monthECDSattendances_includedsites]

drop table [12monthECDSattendances_includedsitesv2]

SELECT [TOKEN_PERSON_ID],[ARRIVAL_DATE], [SITE],[PROVIDER_CODE_DERIVED],[Provider_derived] , [minarrival],[ECDS_bloods_any],[included_sites],[Sentinel_sites] ,[London_sites] ,[outside_sites] ,[five_included_site] ,[Sex]
      , [age] ,[age_group]  ,[age_group2]  ,[ethnic_group] ,[IMD]  ,[live_HCV]  ,[live_HBV]  ,[live_HIV]  ,[HCV]  ,[HBV]  ,[HIV]  ,[HCVresult] ,[HBVresult]  ,[HIVresult]
      ,[HCVAb]  ,[HCVPCR]  ,[HBVAg]  ,[HBVVL]
	  INTO [12monthECDSattendances_includedsitesv2]
  FROM [Y006_BBV_PID].[dbo].[12monthECDSattendancesv2]
  WHERE [included_sites] = 'Yes'
  GROUP BY [TOKEN_PERSON_ID],[ARRIVAL_DATE],[SITE],[PROVIDER_CODE_DERIVED],[Provider_derived] ,[minarrival],[ECDS_bloods_any],[included_sites],[Sentinel_sites] ,[London_sites] ,[outside_sites] ,[five_included_site] ,[Sex]
     , [age],[age_group]  ,[age_group2]  ,[ethnic_group] ,[IMD]  ,[live_HCV]  ,[live_HBV]  ,[live_HIV]  ,[HCV]  ,[HBV]  ,[HIV]  ,[HCVresult] ,[HBVresult]  ,[HIVresult]
      ,[HCVAb]  ,[HCVPCR]  ,[HBVAg]  ,[HBVVL];


-- [12monthECDSattendees_includedsites]

drop table [12monthECDSattendees_includedsitesv2]

SELECT [TOKEN_PERSON_ID]
	INTO [12monthECDSattendees_includedsitesv2]
  FROM [Y006_BBV_PID].[dbo].[12monthECDSattendances_includedsitesv2]
  GROUP BY [TOKEN_PERSON_ID];

  ALTER TABLE [12monthECDSattendees_includedsitesv2]
  ADD arrdate date
  , [ECDS_bloods_any] VARCHAR (255)
  , [Sex] VARCHAR (255)
  , age int
  , [age_group]   VARCHAR (255)
  ,[age_group2]   VARCHAR (255)
  ,[ethnic_group]  VARCHAR (255)
  ,[IMD] VARCHAR (255)

  UPDATE [12monthECDSattendees_includedsitesv2]
  SET [ECDS_bloods_any] =  'Yes'
  FROM [12monthECDSattendees_includedsitesv2]
  INNER JOIN [12monthECDSattendances_includedsitesv2]
  ON [12monthECDSattendances_includedsitesv2].TOKEN_PERSON_ID = [12monthECDSattendees_includedsitesv2].TOKEN_PERSON_ID
  WHERE [12monthECDSattendances_includedsitesv2].[ECDS_bloods_any] is not null;

   With mindate as( SELECT 	[TOKEN_PERSON_ID]
	, MIN([ARRIVAL_DATE]) mindate
	FROM [12monthECDSattendances_includedsitesv2]
	Where [ECDS_bloods_any] is not null
	GROUP BY [TOKEN_PERSON_ID])
	UPDATE [12monthECDSattendees_includedsitesv2]
	SET arrdate = mindate.mindate
	FROM mindate
	INNER JOIN [12monthECDSattendees_includedsitesv2]
	ON [12monthECDSattendees_includedsitesv2].TOKEN_PERSON_ID = mindate.TOKEN_PERSON_ID;


		   With mindate as( SELECT 	[12monthECDSattendances_includedsitesv2].[TOKEN_PERSON_ID]
	, MIN([ARRIVAL_DATE]) mindate
	FROM [12monthECDSattendances_includedsitesv2]
	INNER JOIN [12monthECDSattendees_includedsitesv2]
	ON [12monthECDSattendees_includedsitesv2].TOKEN_PERSON_ID = [12monthECDSattendances_includedsitesv2].TOKEN_PERSON_ID
	Where [12monthECDSattendances_includedsitesv2].[ECDS_bloods_any] is null and arrdate is null
	GROUP BY [12monthECDSattendances_includedsitesv2].[TOKEN_PERSON_ID])
	UPDATE [12monthECDSattendees_includedsitesv2]
	SET arrdate = mindate.mindate
	FROM mindate
	INNER JOIN [12monthECDSattendees_includedsitesv2]
	ON [12monthECDSattendees_includedsitesv2].TOKEN_PERSON_ID = mindate.TOKEN_PERSON_ID;


	UPDATE [12monthECDSattendees_includedsitesv2]
	SET   [Sex]  = [12monthECDSattendances_includedsitesv2].[sex]
	, [age] = [12monthECDSattendances_includedsitesv2].[age]
  , [age_group] = [12monthECDSattendances_includedsitesv2].[age_group]
  ,[age_group2] = [12monthECDSattendances_includedsitesv2].[age_group2]
  ,[ethnic_group]  = [12monthECDSattendances_includedsitesv2].[ethnic_group]
  ,[IMD] = [12monthECDSattendances_includedsitesv2].[IMD]
  FROM [12monthECDSattendees_includedsitesv2]
  INNER JOIN [12monthECDSattendances_includedsitesv2]
  ON [12monthECDSattendees_includedsitesv2].TOKEN_PERSON_ID = [12monthECDSattendances_includedsitesv2].TOKEN_PERSON_ID
  AND [12monthECDSattendees_includedsitesv2].arrdate = [12monthECDSattendances_includedsitesv2].ARRIVAL_DATE;

-- [12monthECDSattendees_5sites]

drop table [12monthECDSattendees_5sitesv2]

SELECT [TOKEN_PERSON_ID]
	INTO [12monthECDSattendees_5sitesv2]
  FROM [Y006_BBV_PID].[dbo].[12monthECDSattendances_includedsitesv2]
  Where [five_included_site] = 'Yes'
  GROUP BY [TOKEN_PERSON_ID];

  ALTER TABLE [12monthECDSattendees_5sitesv2]
  ADD arrdate date
  , [ECDS_bloods_any] VARCHAR (255)
  , [Sex] VARCHAR (255)
  , age int
  , [age_group]   VARCHAR (255)
  ,[age_group2]   VARCHAR (255)
  ,[ethnic_group]  VARCHAR (255)
  ,[IMD] VARCHAR (255);

  UPDATE [12monthECDSattendees_5sitesv2]
  SET [ECDS_bloods_any] =  'Yes'
  FROM [12monthECDSattendees_5sitesv2]
  INNER JOIN [12monthECDSattendances_includedsitesv2]
  ON [12monthECDSattendances_includedsitesv2].TOKEN_PERSON_ID = [12monthECDSattendees_5sitesv2].TOKEN_PERSON_ID
  WHERE [five_included_site] = 'Yes' and [12monthECDSattendances_includedsitesv2].[ECDS_bloods_any] is not null;

   With mindate as( SELECT 	[TOKEN_PERSON_ID]
	, MIN([ARRIVAL_DATE]) mindate
	FROM [12monthECDSattendances_includedsitesv2]
	Where [five_included_site] = 'Yes' and [ECDS_bloods_any] is not null
	GROUP BY [TOKEN_PERSON_ID])
	UPDATE [12monthECDSattendees_5sitesv2]
	SET arrdate = mindate.mindate
	FROM mindate
	INNER JOIN [12monthECDSattendees_5sitesv2]
	ON [12monthECDSattendees_5sitesv2].TOKEN_PERSON_ID = mindate.TOKEN_PERSON_ID;


		   With mindate as( SELECT 	[12monthECDSattendances_includedsitesv2].[TOKEN_PERSON_ID]
	, MIN([ARRIVAL_DATE]) mindate
	FROM [12monthECDSattendances_includedsitesv2]
	INNER JOIN [12monthECDSattendees_5sitesv2]
	ON [12monthECDSattendees_5sitesv2].TOKEN_PERSON_ID = [12monthECDSattendances_includedsitesv2].TOKEN_PERSON_ID
	Where [five_included_site] = 'Yes' and [12monthECDSattendances_includedsitesv2].[ECDS_bloods_any] is null and arrdate is null
	GROUP BY [12monthECDSattendances_includedsitesv2].[TOKEN_PERSON_ID])
	UPDATE [12monthECDSattendees_5sitesv2]
	SET arrdate = mindate.mindate
	FROM mindate
	INNER JOIN [12monthECDSattendees_5sitesv2]
	ON [12monthECDSattendees_5sitesv2].TOKEN_PERSON_ID = mindate.TOKEN_PERSON_ID;


	UPDATE [12monthECDSattendees_5sitesv2]
	SET   [Sex]  = [12monthECDSattendances_includedsitesv2].[sex]
	, age = [12monthECDSattendances_includedsitesv2].[age]
  , [age_group] = [12monthECDSattendances_includedsitesv2].[age_group]
  ,[age_group2] = [12monthECDSattendances_includedsitesv2].[age_group2]
  ,[ethnic_group]  = [12monthECDSattendances_includedsitesv2].[ethnic_group]
  ,[IMD] = [12monthECDSattendances_includedsitesv2].[IMD]
  FROM [12monthECDSattendees_5sitesv2]
  INNER JOIN [12monthECDSattendances_includedsitesv2]
  ON [12monthECDSattendees_5sitesv2].TOKEN_PERSON_ID = [12monthECDSattendances_includedsitesv2].TOKEN_PERSON_ID
  AND [12monthECDSattendees_5sitesv2].arrdate = [12monthECDSattendances_includedsitesv2].ARRIVAL_DATE
  WHERE [five_included_site] = 'Yes';

  SELECT * FROM [12monthECDSattendances_includedsitesv2]

    SELECT * FROM [12monthECDSattendees_5sitesv2]

-- [12monthECDSattendeeswithblood_includedsites]

	  SELECT [TOKEN_PERSON_ID],[ARRIVAL_DATE],[SITE],[PROVIDER_CODE_DERIVED],[Provider_derived] ,[minarrival],[ECDS_bloods_any],[included_sites],[Sentinel_sites] ,[London_sites] ,[outside_sites] ,[five_included_site] ,[Sex]
     ,[age] ,[age_group]  ,[age_group2]  ,[ethnic_group] ,[IMD]  ,[live_HCV]  ,[live_HBV]  ,[live_HIV]  ,[HCV]  ,[HBV]  ,[HIV]  ,[HCVresult] ,[HBVresult]  ,[HIVresult]
      ,[HCVAb]  ,[HCVPCR]  ,[HBVAg]  ,[HBVVL]
	INTO [12monthECDSattendeeswithblood_includedsitesv2]
  FROM [Y006_BBV_PID].[dbo].[12monthECDSattendances_includedsitesv2]
  WHERE [minarrival] = 'Yes'
  GROUP BY [TOKEN_PERSON_ID],[ARRIVAL_DATE],[SITE],[PROVIDER_CODE_DERIVED],[Provider_derived] ,[minarrival],[ECDS_bloods_any],[included_sites],[Sentinel_sites] ,[London_sites] ,[outside_sites] ,[five_included_site] ,[Sex]
     ,[age] ,[age_group]  ,[age_group2]  ,[ethnic_group] ,[IMD]  ,[live_HCV]  ,[live_HBV]  ,[live_HIV]  ,[HCV]  ,[HBV]  ,[HIV]  ,[HCVresult] ,[HBVresult]  ,[HIVresult]
      ,[HCVAb]  ,[HCVPCR]  ,[HBVAg]  ,[HBVVL];