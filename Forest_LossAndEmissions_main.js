/* This code is ACRS 2021 paper entitled, ANALYSIS OF FOREST COVER CHANGES AND FOREST GHG EMISSIONS IN THE PHILIPPINES FROM 2011-2020 USING SATELLITE-DERIVED PRODUCTS" by Fallarcuna et al., (2021)  
This paper aimed to produce annual forest loss trends for all
the 81, Philppine provinces, (Davao Occidental is still lumped with Davao del Sur, NCR: treated as individual province*/

//defining Hasen Global Forest Cover Change Data variable
var hansen_data = ee.Image('UMD/hansen/global_forest_change_2020_v1_8');
//print('Provincial Boundaries', prov_ph);
//print('ph_treecov10',ph_treecov10);
//print('ph_NAM10_for',ph_NAM10_for);

//Preparation of palette (see https://github.com/gee-community/ee-palettes), for visualization options
var palettes = require('users/gena/packages:palettes');
var paired_10 = palettes.colorbrewer.Paired[10];

//Combining the treecov2010 (>=80%) and NAMRIA2010 (forests only)
var ph_NAM10Forests_mask = ee.Feature(ph_NAM10_for);// NAMRIA 2010 forests vector, uploaded shp file in GEE asset 
var ph_10trcv_10NAMfor = ph_treecov10.clip(ph_NAM10Forests_mask);
/*ph_treecov10.clip: represents the 80-100% treecover bindary raster (80-100 as 1, noData as 0), 
uploaded raster file in GEE Assets*/
//print('ph_10trcv_10NAMfor',ph_10trcv_10NAMfor);
//Map.addLayer(ph_10trcv_10NAMfor, {min:0,max:1,palette:paired_10}, 'ph_10trcv_10NAMfor');

//Using the datamask
var datamask = hansen_data.select('datamask');
var hansen_latest = hansen_data.updateMask(datamask);

//Load individual updated/clipeed hansenlayers 
var lossyear = hansen_latest.select('lossyear');

//print('treecover_80_100_NAM_2010', treecover_80_100_NAM_2010);
var lossyear80_100 = lossyear.updateMask(ph_10trcv_10NAMfor);

//These are for visualization purposes
//Adding the masked (80-100% treecover) lossyear
//Map.setCenter(122.314934, 17.098984, 10);
//Map.setOptions('SATELLITE');
//print('lossyear80-100', lossyear80_100);
//Map.addLayer(lossyear80_100, {min:1,max:20,palette:paired_10}, 'lossyear_80-100%');

//Masking the 2011-2010 loss years, retaining the loss years for 2011-2020
var Mask2011_20 =  lossyear80_100.gte(11).and(lossyear80_100.lte(20));//binary result

var lossyear_11to20 = lossyear80_100.updateMask(Mask2011_20);

//Masking the 2011-2020 individual lossyears
var Mask_11 = lossyear_11to20.eq(11);
var lossyear_11 = lossyear_11to20.updateMask(Mask_11).neq(0);
var Mask_12 =  lossyear_11to20.eq(12);
var lossyear_12 = lossyear_11to20.updateMask(Mask_12).neq(0);
var Mask_13 =  lossyear_11to20.eq(13);
var lossyear_13 = lossyear_11to20.updateMask(Mask_13).neq(0);
var Mask_14 =  lossyear_11to20.eq(14);
var lossyear_14 = lossyear_11to20.updateMask(Mask_14).neq(0);
var Mask_15 =  lossyear_11to20.eq(15);
var lossyear_15 = lossyear_11to20.updateMask(Mask_15).neq(0);
var Mask_16 =  lossyear_11to20.eq(16);
var lossyear_16 = lossyear_11to20.updateMask(Mask_16).neq(0);
var Mask_17 =  lossyear_11to20.eq(17);
var lossyear_17 = lossyear_11to20.updateMask(Mask_17).neq(0);
var Mask_18 =  lossyear_11to20.eq(18);
var lossyear_18 = lossyear_11to20.updateMask(Mask_18).neq(0);
var Mask_19 =  lossyear_11to20.eq(19);
var lossyear_19 = lossyear_11to20.updateMask(Mask_19).neq(0);
var Mask_20 =  lossyear_11to20.eq(20);
var lossyear_20 = lossyear_11to20.updateMask(Mask_20).neq(0);

//Concatenate lossyear bands to multiband annual loss
var annual_loss_cat = ee.Image.cat([lossyear_11,
lossyear_12,
lossyear_13,
lossyear_14,
lossyear_15,
lossyear_16,
lossyear_17,
lossyear_18,
lossyear_19,
lossyear_20])
.select([
'lossyear',
'lossyear_1',
'lossyear_2',
'lossyear_3',
'lossyear_4',
'lossyear_5',
'lossyear_6',
'lossyear_7',
'lossyear_8',
'lossyear_9'],['2011',
'2012',
'2013',
'2014',
'2015',
'2016',
'2017',
'2018',
'2019',
'2020']);
//print('2010_annual_loss_cat', annual_loss_cat);

//Converting the bands pixels to hectarage
var annual_lossArea = annual_loss_cat.multiply(ee.Image.pixelArea()).divide(10000);//.reproject({crs:'EPSG:32651',scale:25});
//print('2010_annual_lossArea',annual_lossArea);

var all_prov = ee.FeatureCollection(prov_ph);// "prov_ph" the GADM provincial boundary data for the Philippines
//print('2010_all_prov', all_prov);
//charting of annual forest loss time series 
var chart = ui.Chart.image.byRegion({
                  image: annual_lossArea,
                  regions: all_prov,
                  reducer: ee.Reducer.sum(),
                  scale: 25,
                  xProperty: 'NAME_1'
                })
.setChartType('LineChart')
.setOptions({
  title: 'Philippine Provincial Level Forest Loss',titleTextStyle: {italic: true, bold: true},
  vAxis: {title: '2010_area (in hectares)', titleTextStyle: {italic: false, bold: true}},
  hAxis: {title: 'Year', titleTextStyle: {italic: false, bold: true}}
          });
//print('Provincial Annual Loss',chart);// After printing, csv tables can be saved as serves as input for trend analysis

//charting of baseline forest cover map 
var annual_lossArea_NAM = ph_10trcv_10NAMfor.multiply(ee.Image.pixelArea()).divide(10000);//.reproject({crs:'/EPSG:32651',scale:25});

//charting of annual loss per province
var chart2 = ui.Chart.image.byRegion({
  image: annual_lossArea_NAM,
  regions: all_prov,
  reducer: ee.Reducer.sum(),
  scale: 25,
  xProperty: 'NAME_1'
})
.setChartType('ColumnChart')
.setOptions({
  title: 'Provincial 2010 Base Forest Cover ',titleTextStyle: {italic: true, bold: true},
  vAxis: {title: '2010_area (in hectares)', titleTextStyle: {italic: false, bold: true}},
  hAxis: {title: 'Provinces', titleTextStyle: {italic: false, bold: true}}
          });
//print(chart2);//

/*This section will estimate the C biomass stock 
based from Baccini, 2012 & Spawn, 2020 (Available in GEE) and emissions of Harris,2021 (Available in Global Forest Watch)*/

/*/This lines are used when using Spawn biomass (not used in the study)    
var biomass_spawn = ee.ImageCollection("NASA/ORNL/biomass_carbon_density/v1").select(['agb','bgb']);
var biomass = biomass_spawn.toBands();
var biomass = (biomass.select('2010_agb').add(biomass.select('2010_bgb')))
                      .select('2010_agb').rename('biomTotal');*/

//This line uses when using Baccini biomass (not used in the study) 
//var biomass = ee.Image('WHRC/biomass/tropical').select(['Mg'],['biomTotal']);

//This line uses Harris data ha and px
var biomass = ph_emsPx.select(['b1'],['biomTotal']);//.toUint16();

//This lines uses the agb live woody biomass from Global Forest Watch (Zarin, 2016), not used in the study
//var biomasswoody = ee.Image(ph_agb_liveWoody);
//print('biomass', biomass);                                                                                                     

//printing/checkingall the min max/or any raster variable
print('minmax',biomass.reduceRegion({
      reducer: ee.Reducer.minMax(),
      geometry: Pasa,// sample Area of Interest, declared in GEE Assets
      scale : 25
      }));

//mask the biomass or biomass_combined using the losses from 2011-2020
var biomassImage = biomass.updateMask(lossyear_11to20);
print('biomassImage', biomassImage);

var bioMassImage_11 = biomassImage.updateMask(lossyear_11);
//Map.addLayer(bioMassImage_11, {min:445,max:759,palette:paired_10}, '2010_biomassImage_11');
//print('2010_bioMassImage_11',bioMassImage_11);
var bioMassImage_12 = biomassImage.updateMask(lossyear_12);
var bioMassImage_13 = biomassImage.updateMask(lossyear_13);
var bioMassImage_14 = biomassImage.updateMask(lossyear_14);
var bioMassImage_15 = biomassImage.updateMask(lossyear_15);
var bioMassImage_16 = biomassImage.updateMask(lossyear_16);
var bioMassImage_17 = biomassImage.updateMask(lossyear_17);
var bioMassImage_18 = biomassImage.updateMask(lossyear_18);
var bioMassImage_19 = biomassImage.updateMask(lossyear_19);
var bioMassImage_20 = biomassImage.updateMask(lossyear_20);

var biom = ee.Image.cat([
bioMassImage_11,
bioMassImage_12,
bioMassImage_13,
bioMassImage_14,
bioMassImage_15,
bioMassImage_16,
bioMassImage_17,
bioMassImage_18,
bioMassImage_19,
bioMassImage_20
])
.select(['biomTotal','biomTotal_1','biomTotal_2','biomTotal_3','biomTotal_4','biomTotal_5','biomTotal_6','biomTotal_7','biomTotal_8','biomTotal_9'],
['2011','2012','2013','2014','2015','2016','2017','2018','2019','2020']);
//print('biom', biom);//

//charting of annual biomass loss per province
var chart3 = ui.Chart.image.byRegion({
  image: biom,
  regions: all_prov,
  reducer: ee.Reducer.sum(),
  scale: 25,
  xProperty: 'NAME_1'
})
.setChartType('LineChart')
.setOptions({
  title: 'Forest Carbon Emissions',titleTextStyle: {italic: true, bold: true},
  vAxis: {title: 'metric tons of CO2 equivalent', titleTextStyle: {italic: false, bold: true}},
  hAxis: {title: 'Provinces', titleTextStyle: {italic: false, bold: true}}
          });
print(chart3);

//charting of annual forest biomass loss time series 
var chart4 = ui.Chart.image.regions({
                  image: biom,
                  regions: all_prov,
                  reducer: ee.Reducer.sum(),
                  scale: 25,
                  seriesProperty: 'NAME_1',
                  //xLabels: xPropVals
                })
.setChartType('LineChart')
.setOptions({
  title: 'Forest Carbon Emissions',titleTextStyle: {italic: true, bold: true},
  vAxis: {title: 'metric tons of CO2 equivalent', titleTextStyle: {italic: false, bold: true}},
  hAxis: {title: 'Year', titleTextStyle: {italic: false, bold: true}}
          });
//print(chart4);

//charting of baseline forest cover map to estimate proportional change of emissions (not used in the study)
//var biomass2010 = biomassImage.select(['2010_agb','2010_bgb']);
var biomass2010Area = biomass.clip(ph_NAM10Forests_mask).multiply(ee.Image.pixelArea()).divide(10000);//.reproject({crs:'EPSG:3265'});

//charting of annual forest carbon emissions per province
var chart5 = ui.Chart.image.byRegion({
  image: biomass2010Area,
  regions: all_prov,
  reducer: ee.Reducer.sum(),
  scale: 25,
  xProperty: 'NAME_1'
})
.setChartType('ColumnChart')
.setOptions({
  title: 'Provincial Forest Carbon Emissions in 2010',titleTextStyle: {italic: true, bold: true},
  vAxis: {title: 'metric tons of CO2 equivalent', titleTextStyle: {italic: false, bold: true}},
  hAxis: {title: 'Provinces', titleTextStyle: {italic: false, bold: true}}
          });
print(chart5);

//Exporting of specific area of interest (AOI)
/*Export the image, specifying scale and region.
Export.image.toDrive({
  image: bioMassImage_11,
  description: '2010_bioMassImage11_Abuanfloat',
  scale: 25,
  region: Abuan //sample AOI
});*/