#!/usr/bin/env bash

DIR_BASE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DIR_DATOS="$DIR_BASE/data"
ARCHIVO_USUARIOS="$DIR_DATOS/usuarios.db"      # formato: usuario:contrasena
ARCHIVO_SESION="$DIR_DATOS/sesion.db"         # contiene el usuario logueado actualmente (vacío si nadie)
ARCHIVO_PRODUCTOS="$DIR_DATOS/productos.db"   # formato por línea: CODIGO|TIPO|MODELO|DESCRIPCION|CANTIDAD|PRECIO

# Tipos de pintura
TIPOS=("Base" "Layer" "Shade" "Dry" "Contrast" "Technical" "Texture" "Mediums")

# Crear archivos y estructura inicial
crear_archivos() {
    mkdir -p "$DIR_DATOS"
    touch "$ARCHIVO_USUARIOS" "$ARCHIVO_SESION" "$ARCHIVO_PRODUCTOS"
    # Crear usuario admin por defecto si no existe
    if ! grep -q "^admin:" "$ARCHIVO_USUARIOS" 2>/dev/null; then
        echo "admin:admin" >> "$ARCHIVO_USUARIOS"
    fi
}

# Verificar si un tipo es válido
tipo_valido() {
    local t="$1"
    for x in "${TIPOS[@]}"; do
        if [ "$x" = "$t" ]; then
            return 0
        fi
    done
    return 1
}

# Devuelve el usuario actualmente logueado (vacío si ninguno)
usuario_actual() {
    if [ -s "$ARCHIVO_SESION" ]; then
        cat "$ARCHIVO_SESION"
    else
        echo ""
    fi
}

# Crear nuevo usuario
crear_usuario() {
    while true; do
        read -p "Nombre de usuario: " nombre_usuario
        if grep -q "^$nombre_usuario:" "$ARCHIVO_USUARIOS"; then
            echo "Error: el usuario '$nombre_usuario' ya existe. Intente con otro por favor."
        else
            echo
            read -s -p "Contrasena: " contrasena
            echo
            read -s -p "Confirmar contrasena: " contrasena2
            echo
            if [ -z "$contrasena" ]; then
                echo "La contrasena no puede ser vacía."
            elif [ "$contrasena" != "$contrasena2" ]; then
                echo "Las contrasenas no coinciden. Intente de nuevo."
            else
                break
            fi
        fi
    done
    echo "$nombre_usuario:$contrasena" >> "$ARCHIVO_USUARIOS"
    echo "Usuario '$nombre_usuario' creado con éxito."
}

# Cambiar contrasena del usuario logueado
cambiar_contrasena() {
    usuario_act="$(usuario_actual)"
    if [ -z "$usuario_act" ]; then
        echo "Necesitas logearte para cambiar la contrasena."
        return
    fi

    while true; do
        echo "Cambiando la contrasena de '$usuario_act'"
        read -s -p "Contrasena actual: " contrasena_actual
        echo

        contrasena_guardada="$(grep "^$usuario_act:" "$ARCHIVO_USUARIOS" | cut -d: -f2-)"

        if [ "$contrasena_actual" != "$contrasena_guardada" ]; then
            echo "Contrasena actual incorrecta."
            return
        fi

        read -s -p "Nueva contrasena: " nueva_contrasena
        echo
        read -s -p "Confirmar nueva contrasena: " nueva_contrasena2
        echo

        if [ -z "$nueva_contrasena" ]; then
            echo "La nueva contrasena no puede ser vacía."
        elif [ "$nueva_contrasena" != "$nueva_contrasena2" ]; then
            echo "Las contrasenas no coinciden. Intente otra vez."
        else
            # Reemplazar contrasena en el archivo
            sed -i "s/^$usuario_act:.*/$usuario_act:$nueva_contrasena/" "$ARCHIVO_USUARIOS"
            echo "Contrasena actualizada con éxito."
            break
        fi
    done
}

# Login de usuario
login_usuario() {
    if [ -n "$(usuario_actual)" ]; then
        echo "Ya hay un usuario logueado: $(usuario_actual). Haga logout antes."
        return
    fi

    while true; do
        read -p "Usuario: " usuario_int
        read -s -p "Contrasena: " contrasena_int
        echo

        contrasena_guardada="$(grep "^$usuario_int:" "$ARCHIVO_USUARIOS" | cut -d: -f2-)"

        if [ -z "$contrasena_guardada" ]; then
            echo "Usuario no encontrado."
            return
        fi

        if [ "$contrasena_int" = "$contrasena_guardada" ]; then
            echo "$usuario_int" > "$ARCHIVO_SESION"
            echo "BIENVENIDO: $usuario_int"
            return
        else
            echo "Contrasena incorrecta."
        fi
    done
}

# Logout del usuario actual
logout_usuario() {
    if [ ! -n "$(usuario_actual)" ]; then
        echo "No hay ningún usuario logeado."
        return
    fi
    echo "" > "$ARCHIVO_SESION"
    echo "Logout exitoso."
}

# Ingresar o actualizar un producto en inventario
ingresar_producto() {
    usuario_act="$(usuario_actual)"
    if [ -z "$usuario_act" ]; then
        echo "Necesitas logearte para ingresar un producto."
        return
    fi

    echo "Ingreso de nuevo producto"
    echo "Tipos válidos: ${TIPOS[*]}"

    read -p "Tipo: " tipo
    if ! tipo_valido "$tipo"; then
        echo "Tipo inválido. Operación cancelada."
        return
    fi

    read -p "Modelo (nombre): " modelo
    if [ -z "$modelo" ]; then
        echo "El modelo no puede estar vacío."
        return
    fi

    read -p "Descripción breve: " descripcion

    while true; do
        read -p "Cantidad (entero >=0): " cantidad
        if [[ "$cantidad" =~ ^[0-9]+$ ]]; then break
        else echo "Cantidad inválida."; fi
    done

    while true; do
        read -p "Precio unitario (entero): " precio
        if [[ "$precio" =~ ^[0-9]+$ ]]; then break
        else echo "Precio inválido."; fi
    done

    codigo_tipo="$(echo "$tipo" | cut -c1-3 | tr '[:lower:]' '[:upper:]')"

    archivo_tmp="$(mktemp)"
    encontrado=0

    # Leer productos existentes y actualizar si coincide tipo+modelo
    while IFS='|' read -r cod t m d old_cant old_precio; do
        if [ "$t" = "$tipo" ] && [ "$m" = "$modelo" ]; then
            nuevo_cant=$((old_cant + cantidad))
            echo "$cod|$t|$m|$descripcion|$nuevo_cant|$precio" >> "$archivo_tmp"
            encontrado=1
        else
            echo "$cod|$t|$m|$d|$old_cant|$old_precio" >> "$archivo_tmp"
        fi
    done < "$ARCHIVO_PRODUCTOS"

    if [ "$encontrado" -eq 0 ]; then
        printf "%s|%s|%s|%s|%s|%s\n" "$codigo_tipo" "$tipo" "$modelo" "$descripcion" "$cantidad" "$precio" >> "$archivo_tmp"
        echo "Producto nuevo registrado."
    else
        echo "Producto existente actualizado."
    fi

    mv "$archivo_tmp" "$ARCHIVO_PRODUCTOS"

    echo "✅ Estado actual del producto:"
    grep "|$tipo|$modelo|" "$ARCHIVO_PRODUCTOS" || true
}

# Vender/realizar compra de productos (versión interactiva con lista numerada)
vender_producto() {
    usuario_act="$(usuario_actual)"
    if [ -z "$usuario_act" ]; then
        echo "Debes estar logeado para vender productos."
        return
    fi

    if [ ! -s "$ARCHIVO_PRODUCTOS" ]; then
        echo "No hay productos cargados."
        return
    fi

    # Mostrar lista numerada de productos
    echo "Productos disponibles:"
    mapfile -t lista_productos < "$ARCHIVO_PRODUCTOS"

    for i in "${!lista_productos[@]}"; do
        IFS='|' read -r cod tipo mod desc cant precio <<< "${lista_productos[$i]}"
        echo "$((i+1))) $tipo - $mod - \$ $precio (Stock: $cant)"
    done

    # Compra acumulada
    resumen_tipos=()
    resumen_modelos=()
    resumen_cantidades=()
    resumen_totales=()

    while true; do
        read -p "Ingrese número de producto a comprar (0 para finalizar): " num
        if [ "$num" = "0" ]; then
            break
        fi

        if ! [[ "$num" =~ ^[0-9]+$ ]] || (( num < 1 || num > ${#lista_productos[@]} )); then
            echo "Número inválido."
            continue
        fi

        index=$((num-1))
        IFS='|' read -r cod tipo mod desc cant precio <<< "${lista_productos[$index]}"

        if [ "$cant" -eq 0 ]; then
            echo "No hay stock disponible para este producto."
            continue
        fi

        read -p "Cantidad a comprar: " compra
        if ! [[ "$compra" =~ ^[0-9]+$ ]] || (( compra < 1 || compra > cant )); then
            echo "Cantidad inválida o insuficiente stock."
            continue
        fi

        # Restar stock en la lista temporal
        nuevoCant=$((cant - compra))
        lista_productos[$index]="$cod|$tipo|$mod|$desc|$nuevoCant|$precio"

        total=$((precio * compra))
        resumen_tipos+=("$tipo")
        resumen_modelos+=("$mod")
        resumen_cantidades+=("$compra")
        resumen_totales+=("$total")

        echo "Producto agregado a la compra."
    done

    # Si no se compró nada
    if [ ${#resumen_tipos[@]} -eq 0 ]; then
        echo "No se realizó ninguna compra."
        return
    fi

    # Guardar stock actualizado
    printf "%s\n" "${lista_productos[@]}" > "$ARCHIVO_PRODUCTOS"

    echo
    echo "====== RESUMEN DE COMPRA ======"
    total_final=0
    for i in "${!resumen_tipos[@]}"; do
        echo "- ${resumen_tipos[$i]} | ${resumen_modelos[$i]} | Cantidad: ${resumen_cantidades[$i]} | Total: \$ ${resumen_totales[$i]}"
        total_final=$((total_final + resumen_totales[$i]))
    done
    echo "TOTAL A PAGAR: \$ $total_final"
    echo "=============================="
}

# Filtrar y listar productos por tipo (o todos si filtro vacío)
filtrar_productos() {
    if [ ! -s "$ARCHIVO_PRODUCTOS" ]; then
        echo "No hay productos en el inventario."
        return
    fi

    echo "Filtrar por tipo (${TIPOS[*]})"
    read -p "Dejar vacío para ver todos: " filtro

    echo "------ PRODUCTOS ------"

    if [ -z "$filtro" ]; then
        while IFS='|' read -r cod tipo mod desc cant precio; do
            echo "$tipo - $mod - Cant: $cant - Precio: \$ $precio"
        done < "$ARCHIVO_PRODUCTOS"
    else
        grep -i "|$filtro|" "$ARCHIVO_PRODUCTOS" | while IFS='|' read -r cod tipo mod desc cant precio; do
            echo "$tipo - $mod - Cant: $cant - Precio: \$ $precio"
        done
    fi

    echo "-----------------------"
}

# Crear reporte CSV en carpeta Datos
crear_reporteCSV() {
    mkdir -p "$DIR_BASE/Datos"
    REPORTE_CSV="$DIR_BASE/Datos/datos.CSV"

    echo "Código,Tipo,Modelo,Descripción,Cantidad,Precio" > "$REPORTE_CSV"

    while IFS='|' read -r cod tipo mod desc cant precio; do
        # Evitar líneas vacías
        if [ -n "$cod" ]; then
            echo "$cod,$tipo,$mod,$desc,$cant,$precio" >> "$REPORTE_CSV"
        fi
    done < "$ARCHIVO_PRODUCTOS"

    echo "Reporte generado en: Datos/datos.CSV"
}

# Menú principal
main_menu() {
    crear_archivos
    while true; do
        echo "----------------------------------------"
        echo "Sistema de Inventario - Citadel"
        echo "Usuario actual: $(usuario_actual)"
        echo "1) Crear usuario"
        echo "2) Cambiar contrasena"
        echo "3) Login"
        echo "4) Logout"
        echo "5) Ingresar producto"
        echo "6) Vender producto"
        echo "7) Filtrar productos"
        echo "8) Crear reporte CSV (Datos/datos.CSV)"
        echo "0) Salir"
        echo -n "Elija una opción: "
        read opt
        case "$opt" in
            1) crear_usuario ;;
            2) cambiar_contrasena ;;
            3) login_usuario ;;
            4) logout_usuario ;;
            5) ingresar_producto ;;
            6) vender_producto ;;
            7) filtrar_productos ;;
            8) crear_reporteCSV ;;
            0) echo "Saliendo..."; exit 0 ;;
            *) echo "Opción inválida." ;;
        esac
    done
}

# Ejecutar menú principal
main_menu
