import Ember from 'ember';
import { getFlags } from '../utils/flags';
import { getPlayers } from '../utils/players';

const views = {
  'QUESTION': 'QUESTION',
  'ANSWER': 'ANSWER',
  'RESULTS': 'RESULTS',
};

export default Ember.Component.extend({
  view: views.QUESTION,

  init() {
    this._super(...arguments);
    this.flags = getFlags(this.get('numOfFlags'));
    this.players = getPlayers(this.get('numOfPlayers'));

    this.currentPlayerIndex = 0;
    this.currentFlagIndex = 0;
  },

  actions: {
    revealAnswer() {
      this.set('view', views.ANSWER);
    },

    correctAnswer() {
      let currentPlayer = this.get('players')[this.get('currentPlayerIndex')];
      Ember.set(currentPlayer, 'score', currentPlayer.score + 1);

      this.send('moveToNextQuestion');
    },

    moveToNextQuestion() {
      this.incrementProperty('currentFlagIndex');

      let currentPlayerIndex = this.get('currentPlayerIndex');
      this.set('currentPlayerIndex', currentPlayerIndex >= this.get('numOfPlayers') - 1 ? 0 : currentPlayerIndex + 1 );

      if (this.get('currentFlagIndex') >= this.get('flags').length) {
        this.set('view', views.RESULTS);
      } else {
        this.set('view', views.QUESTION);
      }
    }
  }

});
