// export irrigation stats per municipality
// dhemerson.costa@ipam.org.br

// read irrigation layer
var irrigation = ee.Image('');

// read territories
var territory = ee.FeatureCollection('users/dh-conciani/vectors/municipios_cerrado')
  // create numerical ids
  .map(function(feature) {
    return (feature.set({'geocode': ee.Number.parse(feature.get('CD_MUN'))}));
  });

// rasterize
territory = ee.Image().paint(territory, 'geocode').rename('territory');

