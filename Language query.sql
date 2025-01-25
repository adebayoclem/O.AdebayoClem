select 
	[INTERPRETER_LANGUAGE_Description]
  ,[INTERPRETER_LANGUAGE_VALID]
  ,[PREFERRED_SPOKEN_LANGUAGE_Description]
  ,[PREFERRED_SPOKEN_LANGUAGE_VALID]
  from [12monthECDSattendeeswithblood_includedsitesv2]
left join [ECDS].[dbo].[vtECDS] on
[ECDS].[dbo].[vtECDS].[TOKEN_PERSON_ID] = [dbo].[12monthECDSattendeeswithblood_includedsitesv2].[TOKEN_PERSON_ID]
  where HCVPCR like 'positive'

  select top 10 * from [ECDS].[dbo].[vtECDS]

  select distinct [PREFERRED_SPOKEN_LANGUAGE_Description], count(*)
  from [12monthECDSattendeeswithblood_includedsitesv2]
left join [ECDS].[dbo].[vtECDS] on
[ECDS].[dbo].[vtECDS].[TOKEN_PERSON_ID] = [dbo].[12monthECDSattendeeswithblood_includedsitesv2].[TOKEN_PERSON_ID]
  where HBVAg like 'positive'
  group by [PREFERRED_SPOKEN_LANGUAGE_Description]