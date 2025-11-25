const os = require('os');
const express = require('express');
const bodyParser = require('body-parser');
const cors = require('cors');
const { v4: uuidv4 } = require('uuid');
const { engine } = require('express-handlebars');
const path = require('path');
const redis = require('redis');
const app = express();

const redisClient = redis.createClient({
  host: 'redis',
  port: 6379
});

redisClient.on('error', (err) => {
  console.error('Redis error:', err);
});

app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: true }));
app.use(cors());

const hbs = engine({
  helpers: {
    eq: function(a, b) {
      return a === b;
    }
  }
});

app.engine('handlebars', hbs);
app.set('view engine', 'handlebars');
app.set('views', path.join(__dirname, 'views'));

app.use(express.static(path.join(__dirname, 'public')));

async function getTasks() {
  return new Promise((resolve, reject) => {
    redisClient.get('tasks', (err, data) => {
      if (err) return reject(err);
      
      if (data) {
        return resolve(JSON.parse(data));
      } else {
        const initialTasks = [
          { 
            id: uuidv4(), 
            title: 'Estudiar concurrencia y paralelismo', 
            description: 'Diferencias entre hilos y procesos en sistemas multicore', 
            priority: 'alta', 
            status: 'pendiente',
            category: 'conceptos',
            created: new Date().toISOString()
          },
          { 
            id: uuidv4(), 
            title: 'Implementar algoritmo de planificación Round Robin', 
            description: 'Crear simulador de planificación con quantum de 4ms', 
            priority: 'media', 
            status: 'en-progreso',
            category: 'práctica',
            created: new Date().toISOString()
          },
          { 
            id: uuidv4(), 
            title: 'Investigar sistemas de archivos', 
            description: 'Comparar rendimiento entre ext4, NTFS y APFS', 
            priority: 'baja', 
            status: 'pendiente',
            category: 'investigación',
            created: new Date().toISOString()
          }
        ];
        redisClient.set('tasks', JSON.stringify(initialTasks));
        return resolve(initialTasks);
      }
    });
  });
}

// Ruta principal
app.get('/', async (req, res) => {
  try {
    const tasks = await getTasks();
    const systemInfo = {
      hostname: os.hostname(),
      platform: os.platform(),
      arch: os.arch(),
      cpus: os.cpus().length,
      totalMemory: Math.round(os.totalmem() / (1024 * 1024 * 1024)) + ' GB',
      freeMemory: Math.round(os.freemem() / (1024 * 1024 * 1024)) + ' GB',
      uptime: Math.round(os.uptime() / 3600) + ' hours'
    };

    res.render('home', { 
      title: 'Sistema de Gestión de Tareas - SO',
      tasks: tasks,
      systemInfo: systemInfo,
      layout: 'main'
    });
  } catch (err) {
    console.error(err);
    res.status(500).send('Error al cargar las tareas');
  }
});

// API para obtener todas las tareas
app.get('/api/tasks', async (req, res) => {
  try {
    const tasks = await getTasks();
    res.json(tasks);
  } catch (err) {
    console.error(err);
    res.status(500).json({ error: 'Error al obtener las tareas' });
  }
});

app.post('/api/tasks', (req, res) => {
  const { title, description, priority, category } = req.body;
  
  if (!title) {
    return res.status(400).json({ error: 'El título es requerido' });
  }
  
  const newTask = {
    id: uuidv4(),
    title,
    description: description || '',
    priority: priority || 'media',
    status: 'pendiente',
    category: category || 'general',
    created: new Date().toISOString()
  };
  
  redisClient.get('tasks', (err, data) => {
    if (err) {
      console.error(err);
      return res.status(500).json({ error: 'Error al agregar la tarea' });
    }
    
    let tasks = [];
    if (data) {
      tasks = JSON.parse(data);
    }
    
    tasks.push(newTask);
    redisClient.set('tasks', JSON.stringify(tasks));
    
    res.status(201).json(newTask);
  });
});

app.put('/api/tasks/:id', (req, res) => {
  const { id } = req.params;
  const { title, description, priority, status, category } = req.body;
  
  redisClient.get('tasks', (err, data) => {
    if (err) {
      console.error(err);
      return res.status(500).json({ error: 'Error al actualizar la tarea' });
    }
    
    if (!data) {
      return res.status(404).json({ error: 'No hay tareas registradas' });
    }
    
    let tasks = JSON.parse(data);
    const taskIndex = tasks.findIndex(task => task.id === id);
    
    if (taskIndex === -1) {
      return res.status(404).json({ error: 'Tarea no encontrada' });
    }
    
    tasks[taskIndex] = {
      ...tasks[taskIndex],
      title: title || tasks[taskIndex].title,
      description: description !== undefined ? description : tasks[taskIndex].description,
      priority: priority || tasks[taskIndex].priority,
      status: status || tasks[taskIndex].status,
      category: category || tasks[taskIndex].category,
      updated: new Date().toISOString()
    };
    
    redisClient.set('tasks', JSON.stringify(tasks));
    
    res.json(tasks[taskIndex]);
  });
});

app.delete('/api/tasks/:id', (req, res) => {
  const { id } = req.params;
  
  redisClient.get('tasks', (err, data) => {
    if (err) {
      console.error(err);
      return res.status(500).json({ error: 'Error al eliminar la tarea' });
    }
    
    if (!data) {
      return res.status(404).json({ error: 'No hay tareas registradas' });
    }
    
    let tasks = JSON.parse(data);
    const filteredTasks = tasks.filter(task => task.id !== id);
    
    if (filteredTasks.length === tasks.length) {
      return res.status(404).json({ error: 'Tarea no encontrada' });
    }
    
    redisClient.set('tasks', JSON.stringify(filteredTasks));
    
    res.json({ message: 'Tarea eliminada correctamente' });
  });
});

app.get('/api/system-info', (req, res) => {
  const systemInfo = {
    hostname: os.hostname(),
    platform: os.platform(),
    arch: os.arch(),
    cpus: os.cpus(),
    totalMemory: os.totalmem(),
    freeMemory: os.freemem(),
    uptime: os.uptime(),
    loadAvg: os.loadavg()
  };
  
  res.json(systemInfo);
});

app.listen(5000, function() {
    console.log('Sistema de Gestión de Tareas - SO escuchando en el puerto 5000');
});
