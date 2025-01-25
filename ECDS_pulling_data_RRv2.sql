DROP TABLE [data_events];

SELECT 
    [base].record_id,
    user_token,
    [core_db].[dbo].[user_data].[birth_date],
    [core_db].[dbo].[user_data].[user_identifier],
    [core_db].[dbo].[user_data].[location_code],
    [organization_code],
    [site],
    [event_month],
    [event_date],
    [base].[dbo].[base].[fiscal_year],
    [data_index],
    [record_date],
    [record_code_desc],
    [age_at_event],
    CASE 
        WHEN [age_at_event] BETWEEN 0 AND 15 THEN 'age_0_15'
        WHEN [age_at_event] BETWEEN 16 AND 24 THEN 'age_16_24'
        WHEN [age_at_event] BETWEEN 25 AND 34 THEN 'age_25_34'
        WHEN [age_at_event] BETWEEN 35 AND 49 THEN 'age_35_49'
        WHEN [age_at_event] BETWEEN 50 AND 64 THEN 'age_50_64'
        WHEN [age_at_event] BETWEEN 65 AND 79 THEN 'age_65_79'
        WHEN [age_at_event] BETWEEN 80 AND 130 THEN 'age_80_plus'
        ELSE NULL		
    END AS age_group,
    CASE 
        WHEN [age_at_event] BETWEEN 0 AND 15 THEN 'range_0_15'
        WHEN [age_at_event] BETWEEN 16 AND 44 THEN 'range_16_44'
        WHEN [age_at_event] BETWEEN 45 AND 64 THEN 'range_45_64'
        WHEN [age_at_event] BETWEEN 65 AND 130 THEN 'range_65_plus'
        ELSE NULL		
    END AS age_range,
    CASE
        WHEN [demographic_category_desc] = 'TYPE_A' THEN 'GROUP_A'
        WHEN [demographic_category_desc] IN ('TYPE_B1', 'TYPE_B2') THEN 'GROUP_B'
        WHEN [demographic_category_desc] = 'TYPE_C' THEN 'GROUP_C'
        WHEN [demographic_category_desc] = 'TYPE_D' THEN 'GROUP_D'
        WHEN [demographic_category_desc] IN ('TYPE_E1', 'TYPE_E2', 'TYPE_E3') THEN 'GROUP_E'
        WHEN [demographic_category_desc] = 'UNKNOWN' THEN 'UNKNOWN'
        ELSE NULL		
    END AS demographic_group,
    [area_code],
    [lookup_db].[dbo].[area_metrics].[index_decile],
    [demographic_category],
    [demographic_category_desc],
    [attribute_1],
    [language_preference],
    [language_code],
    [status_code],
    [status_valid],
    [additional_code_1],

    CASE 			
        WHEN [organization_code] IN ('ORG1','ORG2','ORG3','ORG4','ORG5') THEN 'REGION_A'		
        WHEN [organization_code] IN ('ORG6','ORG7','ORG8','ORG9','ORG10') THEN 'REGION_B'		
        WHEN [organization_code] IN ('ORG11','ORG12','ORG13','ORG14','ORG15') THEN 'REGION_C'		
        ELSE 'OTHER'  		
    END AS org_region,

    CASE 
        WHEN [organization_code] = 'ORG1' THEN 'FACILITY_1'
        WHEN [organization_code] = 'ORG2' THEN 'FACILITY_2'
        WHEN [organization_code] = 'ORG3' THEN 'FACILITY_3'
        [... Additional facility mappings ...]
        ELSE 'OTHER'  		
    END AS facility_name,

    CASE 
        WHEN site IN ('SITE001', 'SITE002', 'SITE003') THEN 'Yes'
        ELSE 'No'  		
    END AS included_sites,

    CASE 
        WHEN site IN ('SITE001', 'SITE002', 'SITE003') THEN 'Yes'
        ELSE 'No'  		
    END AS primary_sites,

    CASE 
        WHEN record_code_desc IN (
            'TYPE_1',
            'TYPE_2',
            'TYPE_3',
            'TYPE_4'
        ) THEN 'category_1'
        ELSE NULL
    END AS record_category,

    CASE
        WHEN site = 'SITE001' AND event_date >= '2023-01-01' THEN 'Active'
        WHEN site = 'SITE002' AND event_date >= '2023-02-01' THEN 'Active'
        [... Additional date conditions ...]
        ELSE 'Inactive'
    END AS site_status_a,

    CASE
        WHEN site = 'SITE001' AND event_date >= '2023-01-01' THEN '2023-01-01'
        WHEN site = 'SITE002' AND event_date >= '2023-02-01' THEN '2023-02-01'
        [... Additional date assignments ...]
        ELSE NULL
    END AS activation_date_a

INTO [data_events]
FROM [base].[dbo].[main_table]
LEFT JOIN [base].[dbo].[details_table]
    ON [base].[dbo].[main_table].[record_id] = [base].[dbo].[details_table].[record_id]
LEFT JOIN [core_db].[dbo].[user_data]
    ON [base].[dbo].[main_table].[record_id] = [core_db].[dbo].[user_data].[record_id]
LEFT JOIN [lookup_db].[dbo].[area_metrics]
    ON [base].[dbo].[main_table].[area_code] = [lookup_db].[dbo].[area_metrics].[area_code]
WHERE [organization_code] IN (
    'ORG1', 'ORG2', 'ORG3', 'ORG4', 'ORG5'
)
AND [age_at_event] > 15
GROUP BY [all_selected_columns];

-- Add additional columns for analysis
ALTER TABLE [data_events]
ADD metric_1 VARCHAR(255),
    metric_2 VARCHAR(255),
    metric_3 VARCHAR(255),
    metric_result_1 VARCHAR(255),
    metric_result_2 VARCHAR(255),
    metric_result_3 VARCHAR(255);

-- Update metrics based on related data
UPDATE [data_events]
SET metric_1 = 'Yes',
    metric_1_diff = DATEDIFF(day, [event_date], [record_date])
FROM [dbo].[related_events]
INNER JOIN [data_events]
    ON [related_events].[user_identifier] = [data_events].[user_identifier]
    AND [related_events].[facility_name] = [data_events].[facility_name]
WHERE [related_events].[type] = 'TYPE_1'
    AND DATEDIFF(day, [event_date], [record_date]) BETWEEN 0 AND 7
    AND [data_events].metric_1 IS NULL
    AND [site_status_a] = 'Active';

[... Additional metric updates following similar pattern ...]

-- Create summary tables
SELECT user_token, 
       MIN(event_date) AS first_event,
       COUNT(*) AS total_events,
       SUM(CASE WHEN metric_1 = 'Yes' THEN 1 ELSE 0 END) AS metric_1_count,
       SUM(CASE WHEN metric_2 = 'Yes' THEN 1 ELSE 0 END) AS metric_2_count,
       SUM(CASE WHEN metric_3 = 'Yes' THEN 1 ELSE 0 END) AS metric_3_count
INTO [event_summary]
FROM [data_events]
WHERE included_sites = 'Yes'
GROUP BY user_token;

-- Add demographic and classification columns
ALTER TABLE [event_summary]
ADD demographic_1 VARCHAR(255),
    demographic_2 VARCHAR(255),
    classification_1 VARCHAR(255),
    classification_2 VARCHAR(255);

-- Update summary table with demographic data
UPDATE [event_summary]
SET demographic_1 = de.demographic_group,
    demographic_2 = de.age_group
FROM [event_summary] es
INNER JOIN [data_events] de
    ON es.user_token = de.user_token
    AND es.first_event = de.event_date;

-- Create filtered views for specific analysis
CREATE VIEW [active_sites_view] AS
SELECT *
FROM [data_events]
WHERE site_status_a = 'Active'
    AND included_sites = 'Yes';

CREATE VIEW [primary_sites_view] AS
SELECT *
FROM [data_events]
WHERE primary_sites = 'Yes'
    AND included_sites = 'Yes';