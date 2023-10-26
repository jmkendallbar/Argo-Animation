% Script to create temperature anomaly meridional section figures based on the Roemmich-Gilson
% Argo climatology

% Roemmich-Gilson Argo Climatology (RG) is described here:  https://sio-argo.ucsd.edu/RG_Climatology.html

% The actual grid used in this script takes teh 2004-2018
% RG_Argo_Temperature_Climatology and adds on all the monthly extensions

% This grid is available via ftp here:  https://argo.ucsd.edu/data/argo-data-products/
% It is the 'Scripps Institution of Oceanography' RG product and the link
% wording is 'ftp access for temperature file'  https://sio-argo.ucsd.edu/pub/Global_Marine_Argo_Atlas/RG_ArgoClim_Temp.nc


close all
clear all

% set the path to the folder containing your netcdf files
addpath C:\Users\mscanderbeg\Data\Data\trajectory\data\
addpath C:\Users\mscanderbeg\Data\Data\trajectory\data\animation\
addpath C:\Users\mscanderbeg\Data\Data\trajectory\code\
addpath C:\Users\mscanderbeg\Data\Data\RG\

addpath 'C:\Users\mscanderbeg\Data\Data\trajectory\map_codes\m_map'
addpath 'C:\Users\mscanderbeg\Data\Data\trajectory\map_codes\cmocean'
addpath 'C:\Users\mscanderbeg\Data\Data\trajectory\map_codes\brewer'
addpath 'C:\Users\mscanderbeg\Data\Data\trajectory\csirolib\csirolib\'

set(0,'DefaultFigureVisible','off')
 %set(0,'DefaultFigureVisible','on')
%%%%%%%%%%%%%%%% set filename
% set up incoming file
file_path_in =          'RG_ArgoClim_Temp.nc'

% choose if doing one longitude or an average
avg_flag = 2; % 1 means averaging over longitude, 2 means one longitude 
% 0 means averaging over time

% read all the variables in the nc file and store them in the structure
% data:
% to see what is inside, you type data
% to get to fields inside there, you type:
% data.ctd_temperature
% data.lattiude
f = file_path_in;
ncid = netcdf.open(f,'NC_NOWRITE');
% get info about the file (i.e. var names)
finfo = ncinfo(f)
for j=1:length(finfo(1).Variables(:))
    % var id
    varid = netcdf.inqVarID(ncid,getfield(finfo(1).Variables(j),'Name'));
    % get var
    eval(['data.' getfield(finfo(1).Variables(j),'Name') ...
        ' = squeeze(netcdf.getVar(ncid,varid));'])
end

% load bathy
load etopo2.mat
%%%%%%%%%%%%%%%

cd ..
cd 'Figures/temp_anom_bathyplots'
% set plot directory


% set colorbar
low_lim = -.16;
high_lim = .16;
delta = .08;


% set longitude
lon_pick = -155.5+360;

% set time
month_pick = 1;%228;

% get axes
pres = data.PRESSURE;
lat = data.LATITUDE;
[X,Y] = meshgrid(lat,pres);

% get index
lon_ind = find(data.LONGITUDE == lon_pick);

% % get bathy
btm=interp2(b_lon,b_lat,b_dep,lon_pick*ones(size(double(lat))),double(lat));

% get anomaly
temp = data.ARGO_TEMPERATURE_ANOMALY(lon_ind,:,:,:);
temp_a_s = squeeze(temp);
temp_a_s(temp_a_s == -999) = NaN;

temp_all = data.ARGO_TEMPERATURE_ANOMALY;
temp_all(temp_all == -999) = NaN;

% % get overall min/max for contour levels
% tmi = nanmin(temp_a_s);
% tm = nanmin(squeeze(tmi));
% tmin = nanmin(tm)
%
% tma = nanmax(temp_a_s);
% tmx = nanmax(squeeze(tma));
% tmax = nanmax(tmx)




switch avg_flag
    case 2 
        param_plot = 'RG_temp_anom_animation_csiro_yr_';
        % loop through all months
        [ta tb tc] = size(temp_a_s);
       
        for ff = 1:tc
            temp_a =(temp_a_s(:,:,ff));
            temp_anom = temp_a';

            png_path_out = [plot_dir param_plot num2str(ff) '.png'];           
            print_fig(lat,pres,temp_anom,low_lim,high_lim,delta,png_path_out)
        end
    case 0
        param_plot = 'RG_temp_anom_animation_csiro_p16_year_';
        % loop through all months
        [ta tb tc] = size(temp_a_s);
        year = 2003;
        for ff = 1:12:tc
            if ff <=217
                temp_a = mean(temp_a_s(:,:,ff:ff+11),3);
            elseif ff > 217
                temp_a =mean(temp_a_s(:,:,ff:end),3);
            end
            temp_anom = temp_a';
            year = year + 1;


            %png_path_out = [plot_dir param_plot num2str(ff) '.png'];
            png_path_out = [plot_dir param_plot num2str(year) '.png'];
            print_fig(lat,pres,temp_anom,low_lim,high_lim,delta,png_path_out)
        end
    case 1
        param_plot = 'RG_temp_anom_animation_avg_';
        [ta tb tc td] = size(temp_all);
        for ff = 1:td
            temp_a = squeeze(temp_all(:,:,:,ff));
            ta = nanmean(temp_a,1);
            temp_anom = squeeze(ta)';

            png_path_out = [plot_dir param_plot num2str(ff) '.png'];
            print_fig(lat,pres,temp_anom,low_lim,high_lim,delta,png_path_out)
        end

end

function print_fig(lat,pres,temp_anom,low_lim,high_lim,delta,png_path_out)
                    % set up figure
            % first print entire figure
            f = figure;
            orient landscape
            f.Units = 'inches';
            f.PaperPosition = [.1 .1 10.5 3.5];      
            f.Position = [.1 .1 10.5 3.5];
            % m_proj('miller','lat',[-70 70],'pres',[0 2000]);
            a=axes;
            hold on
            pcolor(lat,pres',temp_anom);
            pbaspect([5 1 1]) % pbaspect([5 1 1])
            %size = [220 44];

            % m_pcolor(lat,pres',temp_a_s);
           % colormap(flipud(brewermap([],'RdYlBu')))
            colormap(clmap(23))
            %colormap((m_colmap('diverging',256))); 
            %coloramp(clmap(23)) 
            shading flat;

            hold on
            %     %add bottom depth
            %     plot(lat,-btm,'k')
            %     %or bottom patch
            %     patch([lat(1),lat',lat(end)],[6000,-btm',6000],[.5 .5 .5])


            clim([low_lim high_lim]) % was -5 5
            c=colorbar('Ticks',[low_lim:delta:high_lim]); % was -5:5:5
            ytickvals = [0,2000,4000, 6000];
            xtickvals = [-60:20:60];
            set(gca,'xtick',xtickvals,'ytick',ytickvals,'Ydir','reverse','FontSize',6);
            set(gca,'box','on','tickdir','out')
            set(gca,'fontsize',14)
            axis([-60 60 0 6000])
            wysiwyg

            %     ps_path_out = [plot_dir param_plot num2str(ff) '.ps'];
            %     print (ps_path_out, '-dpsc','-bestfit')
             print (png_path_out, '-dpng','-r300')

end






