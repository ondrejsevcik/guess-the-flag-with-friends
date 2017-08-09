import shuffle from './shuffle';

export function getPlayers(numberOfPlayers = 3) {
  let names = [
    '😋 Joey',
    '🤠 Ross',
    '👩 Rachel',
    '💆 Monica',
    '🙆 Phoebe'
  ];

  return shuffle(names)
    .splice(0, numberOfPlayers)
    .map(function (name) { return { name: name, score: 0 }});
}
