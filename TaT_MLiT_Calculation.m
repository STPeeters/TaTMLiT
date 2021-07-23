%Calculation of TaT and MLiT by S.T.Peeters and M.E.Kompier
%Last update 21-07-2021
%This script is used to calculate the TaT and MLiT values. Calculations are
%based on our lightlogger data and uses input such as timestamps for which
%to calculate the values, the timespan for which you would like to
%calculate this and finally the thresholds you would like to use. Our light
%loggers provide a .txt file as an output. With each line containing a
%timestamp (in epoch) and the light values measured, in this case
%illuminance (lux). These text files are loaded in this script to do the
%calculations. Also an excel file with the timestamps (in unix) of the
%start of the period for which you would like to calculate these values is
%loaded. The end result is an excel file with all the startPeriod
%timestamps and the calculated values for TaT and MLiT.

% Due to the use of the greenwich epochtimestamp, the data has to be corrected for your timezone
% in wintertime in the Netherlands (GMT +1): input 3600 here
% in summertime in the Netherlands (GMT +2): input 7200 here
timezone = 3600;

% indicate the timespan in hours for which you would like to calculate the
% TaT and MLiT values
timespan = 8.5; %(in hours)

% load the excel file containing the startdate and time (in unix) of the
% periods for which we would like to calculate our parameters. So in the
% end the parameters are calculated for the starttime + timespan. E.g. from
% 08.30 - 17.00. Here the unix times are also transformed to epoch
% timestamps because our light loggers measure in epoch
periodtimes = readtable('LL_PeriodStarts.xlsx'); %%read the table
periodtimestamps = datenum(periodtimes{:,2});

UnixOrigin=datenum('01-01-1970 00:00:00'); %%the epoch starttime
EpochTime=round((periodtimestamps-UnixOrigin)*86400); %%convert unix time to epoch time
EpochTime = EpochTime-timezone;

EpochPeriod = [periodtimes{:,1}, EpochTime]; %%create new array containing all LLnrs and epoch start timestamps

fprintf('Periods loaded. \n')

%% navigate to the directory of the light logger files %%
original_folder = cd;
cd([original_folder,'\DATAFOLDER']) %this folder should contain your light logger data, in our case text files
filelist=dir;
lengte=length(filelist) ; %the number of files plus 2 (. and ..)

%% loop through all thresholds for which we would like to calculate the MLiT and TaT values (log transformed) %%
for threshold = 1.3:0.05:3.3
    
    varNameTaT = ['TaT_', num2str(round(10^threshold))];
    varNameMLiT_epoch = ['MLiT_epoch_', num2str(round(10^threshold))];
    varNameMLiT_rt = ['MLiT_rt_', num2str(round(10^threshold))]; %rt in this case for real time, or unix time
    
    % create empty MAIN table, which will be our Master file with 1 dummy row
    SumData = timetable(datetime((1577878200 + timezone), 'convertfrom', 'posixtime', 'Format', 'yyyy-MM-dd HH:mm:ss'), 1577878200,0,0,1577878200,datetime((1577878200 + timezone), 'convertfrom', 'posixtime', 'Format', 'yyyy-MM-dd HH:mm:ss'),0,0,0,0);
    SumData.Properties.VariableNames = {'EpochTime', 'LLnr', varNameTaT, varNameMLiT_epoch, varNameMLiT_rt,'NaNPercentage','NaNPercentageMorning', 'NaNPercentageAfternoon', 'Existent'};
    
    % loop through all our lightlogger text files in DATAFOLDER
    for n=1:lengte-2
        filename=filelist(n+2).name;
        
        %load LLnr from filename (we used numbers as an identifier)
        LLnr = str2num(filename(11:13)); %%on the basis of our filenames
        
        %load data 
        %time_unix is our timestamp of the lightlogger, Ev is the
        %illuminance measured at that timestamp
        [time_epoch,~,~,~,~,~,Ev ,~, ~, ~,~,~,~,~,~,~,~] = textread(filename, '%f %q %f %f %f %f %f %f %f %f %f %f %f %f %f %f %f','headerlines',1);
        
        %create array with all timestamps which belong to the current
        %lightlogger
        startPeriod = EpochPeriod((EpochPeriod(:,1)==LLnr),:);
        
        %create array with the timestamps of the end of the period for
        %which we want to calculate the parameters which belong to the
        %current lightlogger by using the timespan (in hours)for which we
        %calculate the parameters, which is determined earlier in this
        %script. In this case it was 8.5 hours.
        endPeriod=startPeriod(:,2)+(timespan*60*60);
        
        %search for the lightlogger data between startPeriod and endPeriod
        %check how much of the expected data exists
        %calculate the percentage of missings 
        %Note: missings are actually marked so they do exist as a line of
        %data, whereas the non existent data also has no line of data
        for j= 1:size(startPeriod,1)
            sample = find(time_epoch>=startPeriod(j,2) & time_epoch<endPeriod(j,1));
            
            if (~isempty(sample))
                lightMatrix=[sample time_epoch(sample) Ev(sample) zeros(length(sample),1) zeros(length(sample),1) zeros(length(sample),1) zeros(length(sample),1) zeros(length(sample),1) zeros(length(sample),1) zeros(length(sample),1) zeros(length(sample),1) zeros(length(sample),1)];
                number_of_samples=length(sample);
                
                NaNPercentageMorning = (sum(isnan(lightMatrix(1:floor((size(lightMatrix,1)/2)),3)))/(floor(number_of_samples/2)))*100;
                NaNPercentageAfternoon = (sum(isnan(lightMatrix((1+floor(size(lightMatrix,1)/2)):end,3)))/(1+floor(number_of_samples/2)))*100;
                NaNPercentage = (sum(isnan(lightMatrix(:,3)))/number_of_samples)*100;
                
                %calculate the sample time (in minutes) with which the light was measured
                SampleTime = ((lightMatrix(end,2)-lightMatrix(1,2))/(number_of_samples-1))/60; 
                %we expect 1 extra sample if the difference between the
                %start of the period and the first measured observation +
                %the end of the period and the last measured observation is
                %smaller than the sampletime
                if (lightMatrix(1,2)-(startPeriod(j,2)) + (endPeriod(j,1)-lightMatrix(end,2)) <= SampleTime*60)
                    Existent = 100*number_of_samples/(floor((timespan*60)/SampleTime)+1); 
                else 
                    Existent = 100*number_of_samples/((floor((timespan*60)/SampleTime)));
                end
                
                %calculate the TaT and MLiT parameters, if they meet our
                %requirements with respect to the minimal amount of
                %existing data and the maximum amount of missing data. In
                %our case we set this to at least 70% existing data and a
                %maximum of 30% missing in the morning and afternoon
                for i = 1:size(lightMatrix,1)
                    
                    if(Existent >=70 && NaNPercentageMorning <= 30 && NaNPercentageAfternoon <= 30)
                        
                        if (lightMatrix(i,3) >= threshold)
                            lightMatrix(i,4) = 1;
                            
                        end
                        TaT = 100*(sum(lightMatrix(:,4))/(sum(~isnan(lightMatrix(:,3)))))*(Existent/100);
                        MLiT_epoch = sum(lightMatrix(:,4) .* lightMatrix(:,2)) / sum(lightMatrix(:,4));
                        
                    else
                        TaT = NaN;
                        MLiT_epoch = NaN;
                    end
                end
                
            else
                %if there is no data, we set all parameters to NaN
                X = ['LLnr ', num2str(LLnr), ' has no data for ', num2str(startPeriod(j,2))];
                disp(X)
                TaT = NaN;
                MLiT_epoch = NaN;
                NaNPercentage = NaN;
                NaNPercentageMorning = NaN;
                NaNPercentageAfternoon = NaN;
                Existent = 0;
            end
            
            clear sample
            
            %save parameters in table
            EpochTime = startPeriod(j,2);
            TempSumData = timetable(datetime((startPeriod(j,2)+ timezone), 'convertfrom', 'posixtime', 'Format', 'yyyy-MM-dd HH:mm:ss'), EpochTime, LLnr, TaT,MLiT_epoch,datetime((MLiT_epoch+ timezone), 'convertfrom', 'posixtime', 'Format', 'yyyy-MM-dd HH:mm:ss'),  NaNPercentage,NaNPercentageMorning, NaNPercentageAfternoon, Existent);
            TempSumData.Properties.VariableNames = {'EpochTime', 'LLnr', varNameTaT, varNameMLiT_epoch, varNameMLiT_rt,'NaNPercentage','NaNPercentageMorning', 'NaNPercentageAfternoon', 'Existent'};
            
            %append to master data file
            SumData = [SumData; TempSumData];
            
        end
        
        X = ['Data of LLnr ', num2str(LLnr), ' has been summarized.'];
        disp(X)
    end
    
    cd([original_folder,'\OUTPUTFOLDER'])
    
    %remove dummy row
    SumData(1,:) = [];
    file_title = ['Threshold', num2str(threshold),'.xlsx'];
    %save master data file
    writetable(timetable2table(SumData), file_title);
    cd([original_folder,'\DATAFOLDER'])
    
end
fprintf('Done summarizing all data! \n')