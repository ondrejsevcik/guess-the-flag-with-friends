import Ember from 'ember';
import { getFlags } from '../utils/flags';
import { getPlayers } from '../utils/players';
import Elm from 'guess-the-flag-with-friends/elm-modules'

export default Ember.Component.extend({
  init() {
    this._super(...arguments);
    this.flags = getFlags();
    this.players = getPlayers(this.get('numOfPlayers'));

    this.currentPlayerIndex = 0;
    this.currentFlagIndex = 0;

    this.showResults = false;

    this.Elm = Elm;
  },

  currentFlagUrl: Ember.computed('flags', 'currentFlagIndex', function() {
    return this.get('flags')[this.get('currentFlagIndex')].imgUrl;
  }),
  currentFlagName: Ember.computed('flags', 'currentFlagIndex', function() {
    return this.get('flags')[this.get('currentFlagIndex')].countryName;
  }),

  actions: {
    correct() {
      let currentPlayer = this.get('players')[this.get('currentPlayerIndex')];
      Ember.set(currentPlayer, 'score', currentPlayer.score + 1);

      this.send('moveToNextQuestion');
    },

    moveToNextQuestion() {
      this.incrementProperty('currentFlagIndex');
      // Send update to Elm component
      let imgUrl = this.flags[this.get('currentFlagIndex')].imgUrl;
      console.log(imgUrl);
      this.get('flagUpdated')(imgUrl);

      let currentPlayerIndex = this.get('currentPlayerIndex');
      this.set('currentPlayerIndex', currentPlayerIndex >= this.get('numOfPlayers') - 1 ? 0 : currentPlayerIndex + 1 );

      if (this.get('currentFlagIndex') >= this.get('flags').length) {
        this.toggleProperty('showResults');
      }
    }
  }

});
