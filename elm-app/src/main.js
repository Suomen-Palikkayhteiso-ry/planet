import './main.css'
import { Elm } from './Main.elm'
import lunr from 'lunr'

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

// Load saved selected feed types from localStorage
const savedSelectedFeedTypes = localStorage.getItem('palikkalinkit-selectedFeedTypes') || '["Feed","YouTube","Image"]';

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    timestamp: formattedDate,
    viewMode: savedViewMode,
    selectedFeedTypes: savedSelectedFeedTypes
  }
});

// Handle saving view mode to localStorage
app.ports.saveViewMode.subscribe(function(viewMode) {
  localStorage.setItem('palikkalinkit-viewMode', viewMode);
});

// Handle saving selected feed types to localStorage
app.ports.saveSelectedFeedTypes.subscribe(function(selectedFeedTypes) {
  localStorage.setItem('palikkalinkit-selectedFeedTypes', selectedFeedTypes);
});

// Load search index and set up lunr
let searchIndex = null;
fetch('/search-index.json')
  .then(response => response.json())
  .then(data => {
    searchIndex = lunr(function () {
      this.ref('id');
      this.field('title');
      this.field('description');
      this.field('source');
      data.forEach((item, index) => {
        item.id = index; // Use index as id
        this.add(item);
      });
    });
  })
  .catch(err => console.error('Failed to load search index:', err));

// Handle performSearch
app.ports.performSearch.subscribe(function(query) {
  if (searchIndex) {
    const results = searchIndex.search(query);
    const ids = results.map(result => parseInt(result.ref));
    app.ports.searchResults.send(ids);
  } else {
    app.ports.searchResults.send([]);
  }
});

// Handle scroll to top
app.ports.scrollToTop.subscribe(function() {
  window.scrollTo({ top: 0, behavior: 'smooth' });
});

// Handle scroll to element by ID
app.ports.scrollToElement.subscribe(function(elementId) {
  const element = document.getElementById(elementId);
  if (element) {
    element.scrollIntoView({ behavior: 'smooth', block: 'start' });
  }
});

// Handle focus mobile search
app.ports.focusMobileSearch.subscribe(function() {
  const el = document.getElementById('mobile-search-input');
  if (el) el.focus();
});

// Listen for scroll events
window.addEventListener('scroll', function() {
  app.ports.onScroll.send(window.scrollY);
});
