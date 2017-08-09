import Ember from 'ember';

export default Ember.Controller.extend({
  queryParams: ['numOfPlayers', 'numOfFlags'],
  numOfPlayers: 2, // Default value
  numOfFlags: 50, // Default value
});
