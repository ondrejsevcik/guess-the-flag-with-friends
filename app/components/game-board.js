import Ember from 'ember';
import { getFlags } from '../utils/flags';
import { getPlayers } from '../utils/players';

export default Ember.Component.extend({
  init() {
    this._super(...arguments);
    this.flags = getFlags();
    this.players = getPlayers(this.get('numOfPlayers'));

    this.currentPlayerIndex = 0;
    this.currentFlagIndex = 0;

    this.showQuestion = true;
    this.showResults = false;
  },

  actions: {
    reveal() {
      this.toggleProperty('showQuestion');
    },

    correct() {
      let currentPlayer = this.get('players')[this.get('currentPlayerIndex')];
      Ember.set(currentPlayer, 'score', currentPlayer.score + 1);

      this.send('moveToNextQuestion');
    },

    moveToNextQuestion() {
      this.incrementProperty('currentFlagIndex');

      let currentPlayerIndex = this.get('currentPlayerIndex');
      this.set('currentPlayerIndex', currentPlayerIndex >= this.get('numOfPlayers') - 1 ? 0 : currentPlayerIndex + 1 );

      if (this.get('currentFlagIndex') >= this.get('flags').length) {
        this.toggleProperty('showResults');
      }

      this.toggleProperty('showQuestion');
    }
  }

});
