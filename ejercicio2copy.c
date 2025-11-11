#define _XOPEN_SOURCE 500
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <unistd.h>
#include <time.h>

#define totalPasajeros 100            
#define totalOficinistas 5            
#define lecturasPorPasajero 3      
#define cambiosPorOficinista 3  

// RECURSO COMPARTIDO: El cartel del aeropuerto. Se accede mediante un lock de lectura-escritura (rwlock)
pthread_rwlock_t cartelVuelos;  
int versionCartel = 0;

sem_t mutex;            // Protege las variables de control (lectoresActivos, escritoresEsperando)
sem_t escritura;        // Controla acceso exclusivo de escritores al recurso compartido
sem_t cola;             // Previene inanicion de escritores 

int lectoresActivos = 0;
int escritoresEsperando = 0;

void dormir_milisegundos(unsigned int milisegundos) {
    usleep(milisegundos * 1000);
}

// HILO LECTOR (PASAJERO): el pasajero solo puede leer el cartel, no lo modifica y multiples pasajeros lo pueden leer a la vez.
void *comportamientoPasajero(void *argumento) {
    int idPasajero = *(int *)argumento;
    free(argumento);  // libera memoria del ID recibido
    
    for (int veces = 0; veces < lecturasPorPasajero; ++veces) {
        // Simular que el pasajero esta haciendo otras cosas (checkin, cafe, etc)
        int tiempoAntesMirar = rand() % 1001;
        dormir_milisegundos(tiempoAntesMirar);

        sem_wait(&cola);
        sem_wait(&mutex);
        lectoresActivos++;

        if (lectoresActivos == 1) {
            sem_wait(&escritura);  // Bloquea escritores mientras haya lectores
        }

        sem_post(&mutex);
        sem_post(&cola);
        
        printf("Pasajero %d esta mirando el cartel (version %d)\n", 
               idPasajero, versionCartel);
        fflush(stdout);
        
        // Simular tiempo que tarda en leer la informacion del panel
        int tiempoLeyendoPanel = 50 + (rand() % 151);  
        dormir_milisegundos(tiempoLeyendoPanel);
 
        sem_wait(&mutex);
        lectoresActivos--;

        if (lectoresActivos == 0) {
            sem_post(&escritura);  
        }

        sem_post(&mutex);
    }
    return NULL;
}

// HILO ESCRITOR (OFICINISTA): los oficinistas modifican el cartel, solo uno a la vez y mientras este escribe ningun pasajero puede leer
void *comportamientoOficinista(void *argumento) {
    int idOficinista = *(int *)argumento;
    free(argumento); 

    for (int actualizacion = 0; actualizacion < cambiosPorOficinista; ++actualizacion) {
        // Simular tiempo antes de necesitar actualizar (recibir info de torre, etc)
        int tiempoAntesActualizar = rand() % 1001;  
        dormir_milisegundos(tiempoAntesActualizar);
        
        sem_wait(&mutex);
        escritoresEsperando++;
        sem_post(&mutex);
        sem_wait(&cola);
        sem_wait(&escritura);
        sem_wait(&mutex);
        escritoresEsperando--;
        sem_post(&mutex);

        printf("Oficinista %d esta modificando el cartel\n", idOficinista);
        fflush(stdout);  
        
        // Simular tiempo que tarda en actualizar vuelos, puertas, horarios
        int tiempoActualizandoInfo = 200 + (rand() % 301); 
        dormir_milisegundos(tiempoActualizandoInfo);

        // Modificar el recurso compartido (incrementar version del cartel)
        versionCartel++;
        
        sem_post(&cola);
        sem_post(&escritura);
    }
    return NULL;
}

int main(void) {
    srand((unsigned int)time(NULL));
    
    if (sem_init(&mutex, 0, 1) != 0) {
        perror("sem_init mutex");
        return EXIT_FAILURE;
    }

    if (sem_init(&escritura, 0, 1) != 0) {
        perror("sem_init escritura");
        sem_destroy(&mutex);
        return EXIT_FAILURE;
    }
    
    if (sem_init(&cola, 0, 1) != 0) {
        perror("sem_init cola");
        sem_destroy(&mutex);
        sem_destroy(&escritura);
        return EXIT_FAILURE;
    }
    
    //Declarar arrays para almacenar los identificadores de hilos
    pthread_t hilosPasajeros[totalPasajeros];
    pthread_t hilosOficinistas[totalOficinistas];
    
    printf("=== Sistema de Cartel de Aeropuerto ===\n");
    printf("Pasajeros: %d | Oficinistas: %d\n", totalPasajeros, totalOficinistas);
    printf("Iniciando simulacion...\n\n");
    
    // Crear hilos ESCRITORES = Oficinistas
    for (int i = 0; i < totalOficinistas; ++i) {
        // Asignar memoria dinamica para pasar el ID al hilo, necesario porque el loop cambiaria el valor de 'i'
        int *idOficinista = malloc(sizeof(int));
        if (idOficinista == NULL) {
            perror("malloc oficinista");
            return EXIT_FAILURE;
        }
        *idOficinista = i + 1;
        
        if (pthread_create(&hilosOficinistas[i], NULL, comportamientoOficinista, idOficinista) != 0) {
            perror("pthread_create oficinista");
            free(idOficinista);
            return EXIT_FAILURE;
        }
    }
    
    //Crear hilos LECTORES = Pasajeros
    for (int i = 0; i < totalPasajeros; ++i) {
        int *idPasajero = malloc(sizeof(int));
        if (idPasajero == NULL) {
            perror("malloc pasajero");
            return EXIT_FAILURE;
        }
        *idPasajero = i + 1;
   
        if (pthread_create(&hilosPasajeros[i], NULL, comportamientoPasajero, idPasajero) != 0) {
            perror("pthread_create pasajero");
            free(idPasajero);
            return EXIT_FAILURE;
        }
    }
    
    // Esperar a que TODOS los oficinistas terminen sus actualizaciones
    // pthread_join bloquea hasta que el hilo termine
    for (int i = 0; i < totalOficinistas; ++i) {
        pthread_join(hilosOficinistas[i], NULL);
    }
    
    //Esperar a que TODOS los pasajeros terminen de consultar
    for (int i = 0; i < totalPasajeros; ++i) {
        pthread_join(hilosPasajeros[i], NULL);
    }
    
    sem_destroy(&mutex);
    sem_destroy(&escritura);
    sem_destroy(&cola);

    printf("\n=== Simulacion finalizada ===\n");
    printf("Version final del cartel: %d\n", versionCartel);
    printf("Total de cambios esperados: %d\n", totalOficinistas * cambiosPorOficinista);
    
    return 0;
}
