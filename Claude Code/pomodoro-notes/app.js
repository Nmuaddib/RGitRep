const MODES = {
  work: { label: 'Foco', minutes: 25 },
  short: { label: 'Pausa curta', minutes: 5 },
  long: { label: 'Pausa longa', minutes: 15 },
};

const CIRCUMFERENCE = 2 * Math.PI * 90;

const state = {
  mode: 'work',
  secondsLeft: MODES.work.minutes * 60,
  totalSeconds: MODES.work.minutes * 60,
  running: false,
  intervalId: null,
};

const timerDisplay = document.getElementById('timerDisplay');
const ringProgress = document.querySelector('.ring-progress');
const startBtn = document.getElementById('startBtn');
const pauseBtn = document.getElementById('pauseBtn');
const resetBtn = document.getElementById('resetBtn');
const cycleCountEl = document.getElementById('cycleCount');
const noteInput = document.getElementById('noteInput');
const saveNoteBtn = document.getElementById('saveNoteBtn');
const clearHistoryBtn = document.getElementById('clearHistoryBtn');
const historyList = document.getElementById('historyList');
const dingSound = document.getElementById('dingSound');
const modeBtns = document.querySelectorAll('.mode-btn');

ringProgress.style.strokeDasharray = CIRCUMFERENCE;

function todayKey() {
  return new Date().toISOString().slice(0, 10);
}

function loadHistory() {
  return JSON.parse(localStorage.getItem('pomodoro_history') || '[]');
}

function saveHistory(history) {
  localStorage.setItem('pomodoro_history', JSON.stringify(history));
}

function loadCycleCount() {
  const data = JSON.parse(localStorage.getItem('pomodoro_cycles') || '{}');
  return data[todayKey()] || 0;
}

function incrementCycleCount() {
  const data = JSON.parse(localStorage.getItem('pomodoro_cycles') || '{}');
  const key = todayKey();
  data[key] = (data[key] || 0) + 1;
  localStorage.setItem('pomodoro_cycles', JSON.stringify(data));
  return data[key];
}

function formatTime(totalSeconds) {
  const m = Math.floor(totalSeconds / 60).toString().padStart(2, '0');
  const s = Math.floor(totalSeconds % 60).toString().padStart(2, '0');
  return `${m}:${s}`;
}

function updateDisplay() {
  timerDisplay.textContent = formatTime(state.secondsLeft);
  const fraction = state.secondsLeft / state.totalSeconds;
  ringProgress.style.strokeDashoffset = CIRCUMFERENCE * (1 - fraction);
}

function switchMode(mode) {
  clearInterval(state.intervalId);
  state.running = false;
  state.mode = mode;
  state.totalSeconds = MODES[mode].minutes * 60;
  state.secondsLeft = state.totalSeconds;
  startBtn.disabled = false;
  pauseBtn.disabled = true;
  startBtn.textContent = 'Iniciar';
  modeBtns.forEach(btn => btn.classList.toggle('active', btn.dataset.mode === mode));
  updateDisplay();
}

function tick() {
  state.secondsLeft--;
  updateDisplay();
  if (state.secondsLeft <= 0) {
    completeCycle();
  }
}

function completeCycle() {
  clearInterval(state.intervalId);
  state.running = false;
  startBtn.disabled = false;
  pauseBtn.disabled = true;
  startBtn.textContent = 'Iniciar';
  try { dingSound.play(); } catch (e) {}

  if (state.mode === 'work') {
    const count = incrementCycleCount();
    cycleCountEl.textContent = count;
    noteInput.focus();
  }

  const history = loadHistory();
  history.unshift({
    mode: state.mode,
    label: MODES[state.mode].label,
    text: '',
    timestamp: new Date().toISOString(),
  });
  saveHistory(history);
  renderHistory();
}

function startTimer() {
  if (state.running) return;
  state.running = true;
  startBtn.disabled = true;
  pauseBtn.disabled = false;
  startBtn.textContent = 'Rodando...';
  state.intervalId = setInterval(tick, 1000);
}

function pauseTimer() {
  clearInterval(state.intervalId);
  state.running = false;
  startBtn.disabled = false;
  pauseBtn.disabled = true;
  startBtn.textContent = 'Continuar';
}

function resetTimer() {
  switchMode(state.mode);
}

function saveNote() {
  const text = noteInput.value.trim();
  if (!text) return;
  const history = loadHistory();
  if (history.length > 0 && history[0].text === '') {
    history[0].text = text;
  } else {
    history.unshift({
      mode: state.mode,
      label: MODES[state.mode].label,
      text,
      timestamp: new Date().toISOString(),
    });
  }
  saveHistory(history);
  noteInput.value = '';
  renderHistory();
}

function renderHistory() {
  const history = loadHistory();
  historyList.innerHTML = '';
  if (history.length === 0) {
    historyList.innerHTML = '<li class="empty-state">Nenhum registro ainda. Complete um ciclo ou salve uma nota!</li>';
    return;
  }
  history.forEach(item => {
    const li = document.createElement('li');
    if (item.mode !== 'work') li.classList.add('pause');
    const date = new Date(item.timestamp);
    const timeStr = date.toLocaleString('pt-BR', { day: '2-digit', month: '2-digit', hour: '2-digit', minute: '2-digit' });
    li.innerHTML = `
      <div class="history-meta">${item.label} • ${timeStr}</div>
      <div class="history-text">${item.text ? escapeHtml(item.text) : '<em>(sem nota)</em>'}</div>
    `;
    historyList.appendChild(li);
  });
}

function escapeHtml(str) {
  const div = document.createElement('div');
  div.textContent = str;
  return div.innerHTML;
}

modeBtns.forEach(btn => {
  btn.addEventListener('click', () => switchMode(btn.dataset.mode));
});

startBtn.addEventListener('click', startTimer);
pauseBtn.addEventListener('click', pauseTimer);
resetBtn.addEventListener('click', resetTimer);
saveNoteBtn.addEventListener('click', saveNote);
clearHistoryBtn.addEventListener('click', () => {
  if (confirm('Tem certeza que deseja apagar todo o histórico?')) {
    saveHistory([]);
    renderHistory();
  }
});

noteInput.addEventListener('keydown', (e) => {
  if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
    saveNote();
  }
});

cycleCountEl.textContent = loadCycleCount();
updateDisplay();
renderHistory();
