import DS from 'ember-data';

export default DS.Model.extend({
  flags: DS.hasMany('flag'),
  players: DS.hasMany('player'),
  currentPlayerId: DS.attr()
});
