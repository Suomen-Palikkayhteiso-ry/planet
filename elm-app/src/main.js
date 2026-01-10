import './main.css'
import { Elm } from './Main.elm'

// Format date in Finnish locale with Europe/Helsinki timezone
const now = new Date();
const options = {
  timeZone: 'Europe/Helsinki',
  year: 'numeric',
  month: '2-digit',
  day: '2-digit',
  hour: '2-digit',
  minute: '2-digit',
  second: '2-digit',
  hour12: false
};

const formatted = new Intl.DateTimeFormat('fi-FI', options)
  .formatToParts(now)
  .reduce((acc, part) => {
    if (part.type === 'day') acc.day = part.value;
    if (part.type === 'month') acc.month = part.value;
    if (part.type === 'year') acc.year = part.value;
    if (part.type === 'hour') acc.hour = part.value;
    if (part.type === 'minute') acc.minute = part.value;
    if (part.type === 'second') acc.second = part.value;
    return acc;
  }, {});

const formattedDate = `${formatted.day}.${formatted.month}.${formatted.year} ${formatted.hour}:${formatted.minute}:${formatted.second}`;

// Load saved view mode from localStorage
const savedViewMode = localStorage.getItem('palikkalinkit-viewMode') || 'Full';

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    timestamp: formattedDate,
    viewMode: savedViewMode
  }
});

// Handle saving view mode to localStorage
app.ports.saveViewMode.subscribe(function(viewMode) {
  localStorage.setItem('palikkalinkit-viewMode', viewMode);
});
