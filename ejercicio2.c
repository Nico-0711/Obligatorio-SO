// aeropuerto.c
// Solucion al problema de LECTORES-ESCRITORES usando pthread_rwlock_t
// Escenario: Panel de vuelos de aeropuerto con pasajeros (lectores) y oficinistas (escritores)

#define _XOPEN_SOURCE 500
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <time.h>

#define totalPasajeros 100            
#define totalOficinistas 5            
#define lecturasPorPasajero 3      
#define cambiosPorOficinista 3  

// RECURSO COMPARTIDO: El cartel del aeropuerto. Se accede mediante un lock de lectura-escritura (rwlock)
pthread_rwlock_t cartelVuelos;  
int versionCartel = 0;         


void dormir_milisegundos(unsigned int milisegundos) {
    usleep(milisegundos * 1000);
}

// HILO LECTOR (PASAJERO): el pasajero solo puede leer el cartel, no lo modifica y multiples pasajeros lo pueden leer a la vez.

void *comportamientoPasajero(void *argumento) {
    int idPasajero = *(int *)argumento;
    free(argumento);  // libera memoria del ID recibido
    
    for (int veces = 0; veces < lecturasPorPasajero; ++veces) {
        // Simular que el pasajero esta haciendo otras cosas (checkin, cafe, etc)
        int tiempoAntesMirar = rand() % 3001;
        dormir_milisegundos(tiempoAntesMirar);
        
        // Solicita un bloqueo de lectura (rdlock) que permite múltiples lectores simultáneos pero bloquea el acceso si un oficinista esta escribiendo
        pthread_rwlock_rdlock(&cartelVuelos);
        
        // Lectura del cartel (recurso compartido)
        printf("Pasajero %d esta mirando el cartel (version %d)\n", 
               idPasajero, versionCartel);
        fflush(stdout);  // Forzar impresion inmediata
        
        // Simular tiempo que tarda en leer la informacion del panel
        int tiempoLeyendoPanel = 100 + (rand() % 401);  
        dormir_milisegundos(tiempoLeyendoPanel);
        
        // Libera permiso de lestura
        pthread_rwlock_unlock(&cartelVuelos);

    }
    return NULL;
}

// HILO ESCRITOR (OFICINISTA): los oficinistas modifican el cartel, solo uno a la vez y mientras este escribe ningun pasajero puede leer

void *comportamientoOficinista(void *argumento) {
    int idOficinista = *(int *)argumento;
    free(argumento); 

    for (int actualizacion = 0; actualizacion < cambiosPorOficinista; ++actualizacion) {
        // Simular tiempo antes de necesitar actualizar (recibir info de torre, etc)
        int tiempoAntesActualizar = rand() % 2001;  
        dormir_milisegundos(tiempoAntesActualizar);
        
        // Solicita el permiso de escritura (wrlock), otorgando acceso exclusivo al oficinista y bloqueando tanto a los pasajeros como a otros oficinistas hasta que no haya nadie leyendo ni escribiendo.
        pthread_rwlock_wrlock(&cartelVuelos);
        
        // Modificar el recurso compartido
        printf("Oficinista %d esta modificando el cartel\n", idOficinista);
        fflush(stdout);
        
        // Simular tiempo que tarda en actualizar vuelos, puertas, horarios
        int tiempoActualizandoInfo = 500 + (rand() % 1001); 
        dormir_milisegundos(tiempoActualizandoInfo);

        versionCartel++;
        
        // Libera permiso de escritura, ahora otros pueden acceder ya sea pasajeros u oficinistas
        pthread_rwlock_unlock(&cartelVuelos);
    }
    return NULL;
}

int main(void) {
    srand((unsigned int)time(NULL));
    
    //Inicializar el candado Read-Write Lock, mecanismo de sincronizacion
    if (pthread_rwlock_init(&cartelVuelos, NULL) != 0) {
        perror("pthread_rwlock_init");
        return EXIT_FAILURE;
    }
    
    pthread_t hilosPasajeros[totalPasajeros];
    pthread_t hilosOficinistas[totalOficinistas];
    
    printf("=== Sistema de Cartel de Aeropuerto ===\n");
    printf("Pasajeros: %d | Oficinistas: %d\n", totalPasajeros, totalOficinistas);
    printf("Iniciando simulacion...\n\n");
    
    //Crear hilos ESCRITORES = Ofinicinistas
    for (int i = 0; i < totalOficinistas; ++i) {
        // Asignar memoria dinamica para pasar el ID al hilo, necesario porque el loop cambiaria el valor de 'i'
        int  *idOficinista = malloc(sizeof(int));
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
   
        if (pthread_create(&hilosPasajeros[i], NULL,comportamientoPasajero, idPasajero) != 0) {
            perror("pthread_create pasajero");
            free(idPasajero);
            return EXIT_FAILURE;
        }
    }
    
    //Esperar a que TODOS los oficinistas terminen sus actualizaciones, pthread_join bloquea hasta que el hilo termine
    for (int i = 0; i < totalOficinistas; ++i) {
        pthread_join(hilosOficinistas[i], NULL);
    }
    
    //Esperar a que TODOS los pasajeros terminen de consultar
    for (int i = 0; i < totalPasajeros; ++i) {
        pthread_join(hilosPasajeros[i], NULL);
    }
    
    // PASO 6: Destruir el candado Read-Write Lock (liberar recursos)
    pthread_rwlock_destroy(&cartelVuelos);

    printf("\n=== Simulacion finalizada ===\n");
    printf("Version final del cartel: %d\n", versionCartel);
    printf("Total de cambios esperados: %d\n", totalOficinistas * cambiosPorOficinista);
    
    return 0;
}

/*
 * PROPIEDADES DEL pthread_rwlock_t (Read-Write Lock):
 * 
 * 1. LECTURA CONCURRENTE:
 *    - Multiples pasajeros pueden tener permiso de lectura simultaneamente
 *    - Eficiente cuando hay muchas lecturas y pocas escrituras
 * 
 * 2. ESCRITURA EXCLUSIVA:
 *    - Solo UN oficinista puede tener permiso de escritura a la vez
 *    - Mientras hay escritura activa, NADIE mas puede acceder (ni leer ni escribir)
 * 
 * 3. PREVENCION DE CONDICIONES DE CARRERA:
 *    - Garantiza que versionCartel nunca se lee mientras se modifica
 *    - Evita inconsistencias en los datos del panel
 * 
 * 4. VENTAJA vs MUTEX:
 *    - Un mutex bloquearia TODA operacion (lectura o escritura)
 *    - rwlock permite lecturas simultaneas = mas eficiente para aeropuertos
 */