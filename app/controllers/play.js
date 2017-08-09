import Ember from 'ember';

export default Ember.Controller.extend({
  queryParams: ['numOfPlayers'],
  numOfPlayers: 2, // Default value
});
