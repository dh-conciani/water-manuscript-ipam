// export water surface  stats per basin
// dhemerson.costa@ipam.org.br

// define the years to be computed 
var years = [1985, 2022];

// read water surface
var collection = ee.ImageCollection('projects/mapbiomas-workspace/AMOSTRAS/GTAGUA/OBJETOS/CLASSIFICADOS/TESTE_1_raster')
  .filter(ee.Filter.eq('version', '2'));
  
  
// for each year, convert into image
var recipe = ee.Image([]);  // empty recipe 
years.forEach(function(year_i) {
  // convert into image and store
   recipe = recipe.addBands(
     collection.filter(ee.Filter.eq('year', year_i))
      .mosaic()
      .rename('classification_' + year_i));
});

var irrigation = recipe;

// read territories
var territory = ee.FeatureCollection('users/dh-conciani/vectors/municipios_cerrado')
  // create numerical ids
  .map(function(feature) {
    return (feature.set({'geocode': ee.Number.parse(feature.get('CD_MUN'))}));
  });

// rasterize
territory = ee.Image().paint(territory, 'geocode').rename('territory');

// change the scale if you need.
var scale = 30;

// define a Google Drive output folder 
var driverFolder = 'IRRIGATION';

// get the classification for the file[i] 
var asset_i = irrigation.selfMask();

// Image area in hectares
var pixelArea = ee.Image.pixelArea().divide(10000);

// Geometry to export
var geometry = asset_i.geometry();

// convert a complex object to a simple feature collection 
var convert2table = function (obj) {
  obj = ee.Dictionary(obj);
    var territory = obj.get('territory');
    var classesAndAreas = ee.List(obj.get('groups'));
    
    var tableRows = classesAndAreas.map(
        function (classAndArea) {
            classAndArea = ee.Dictionary(classAndArea);
            var classId = classAndArea.get('class');
            var area = classAndArea.get('sum');
            var tableColumns = ee.Feature(null)
                .set('territory', territory)
                .set('class_id', classId)
                .set('area', area);
                
            return tableColumns;
        }
    );
  
    return ee.FeatureCollection(ee.List(tableRows));
};

// compute the area
var calculateArea = function (image, territory, geometry) {
    var territotiesData = pixelArea.addBands(territory).addBands(image)
        .reduceRegion({
            reducer: ee.Reducer.sum().group(1, 'class').group(1, 'territory'),
            geometry: geometry,
            scale: scale,
            maxPixels: 1e13
        });
        
    territotiesData = ee.List(territotiesData.get('groups'));
    var areas = territotiesData.map(convert2table);
    areas = ee.FeatureCollection(areas).flatten();
    return areas;
};

// perform per year 
var areas = years.map(
    function (year) {
        var image = asset_i.select('classification_' + year);
        var areas = calculateArea(image, territory, geometry);
        // set additional properties
        areas = areas.map(
            function (feature) {
                return feature.set('year', year);
            }
        );
        return areas;
    }
);

areas = ee.FeatureCollection(areas).flatten();
  
Export.table.toDrive({
    collection: areas,
    description: 'waterSurface_cities',
    folder: driverFolder,
    fileFormat: 'CSV'
});
