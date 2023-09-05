#!/bin/bash

##wget datastream daily rf monthly files
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/working_data/hads/$(AGGREGATION_DATE_YESTERDAY_Y_M)_hads_daily_rf.csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/working_data/hads/
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/working_data/madis/$(AGGREGATION_DATE_YESTERDAY_Y_M)_madis_daily_rf.csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/working_data/madis/
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/working_data/nws_rr5/$(AGGREGATION_DATE_YESTERDAY_Y_M)_nws_daily_rf.csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/working_data/nws_rr5/

##wget station monitoring log tables
#missing/unknown stations monthly file
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/rf_station_tracking/missing/$(AGGREGATION_DATE_YESTERDAY_Y_M)_unknown_rf_sta.csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/rf_station_tracking/missing/
#count log 
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/rf_station_tracking/count/$(AGGREGATION_DATE_YESTERDAY_Y_M)_count_log_daily_rf.csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/rf_station_tracking/count/
#gap fill log
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/rf_station_tracking/gapFilling/$(AGGREGATION_DATE_YESTERDAY_Y_M)_gap_fill_log_daily_rf.csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/rf_station_tracking/gapFilling/
#qaqc fail log
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/rf_station_tracking/qaqc_fail/$(AGGREGATION_DATE_YESTERDAY_Y_M)_qaqc_fail_daily_rf.csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/rf_station_tracking/qaqc_fail/

##wget daily rf monthly station files 
#rf station daily source
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/station_data/daily/source/statewide/Statewide_Daily_Source_$(AGGREGATION_DATE_YESTERDAY_Y_M).csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/station_data/daily/source/statewide/
#rf station daily raw rainfall mm
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/station_data/daily/raw/statewide/Statewide_Raw_Daily_RF_mm_$(AGGREGATION_DATE_YESTERDAY_Y_M).csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/station_data/daily/raw/statewide/
#rf station daily qaqc raw rainfall mm
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/station_data/daily/raw_qc/statewide/Statewide_QAQC_Raw_Daily_RF_mm_$(AGGREGATION_DATE_YESTERDAY_Y_M).csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/station_data/daily/raw_qc/statewide/
#rf station daily qaqc flag
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/station_data/daily/qc_flag/statewide/Statewide_RF_QAQC_Daily_Bad_Flag_$(AGGREGATION_DATE_YESTERDAY_Y_M).csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/station_data/daily/qc_flag/statewide/
#rf station daily qaqc prob
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/station_data/daily/qc_prob/statewide/Statewide_RF_QAQC_Daily_Bad_Prob_$(AGGREGATION_DATE_YESTERDAY_Y_M).csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/station_data/daily/qc_prob/statewide/
#rf station daily partial filled rainfall mm
wget -v "https://ikeauth.its.hawaii.edu/files/v2/download/public/system/ikewai-annotated-data/HCDP/workflow_data/preliminary/rainfall/data_outputs/tables/station_data/daily/partial_filled/statewide/Statewide_Partial_Filled_Daily_RF_mm_$(AGGREGATION_DATE_YESTERDAY_Y_M).csv" -P /home/hawaii_climate_products_container/preliminary/rainfall/data_outputs/tables/station_data/daily/partial_filled/


