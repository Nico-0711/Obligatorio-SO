#!/usr/bin/env bash


BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DATA_DIR="$BASE_DIR/data"
USERS_FILE="$DATA_DIR/users.db"      # formato: usuario:contraseña
SESSION_FILE="$DATA_DIR/session.db"  # contiene el usuario logueado actualmente (vacío si nadie)
PRODUCTS_FILE="$DATA_DIR/products.db" # formato por línea: CÓDIGO|TIPO|MODELO|DESCRIPCION|CANTIDAD|PRECIO


crear_archivos() {
    mkdir -p "$DATA_DIR"
    touch "$USERS_FILE" "$SESSION_FILE" "$PRODUCTS_FILE"
    # Crear usuario admin por defecto si no existe
    if ! grep -q "^admin:" "$USERS_FILE" 2>/dev/null; then
        echo "admin:admin" >> "$USERS_FILE"
    fi
}

TYPES=("Base" "Layer" "Shade" "Dry" "Contrast" "Technical" "Texture" "Mediums")

valid_type() {
    local t="$1"
    for x in "${TYPES[@]}"; do
        if [ "$x" = "$t" ]; then
            return 0
        fi
    done
    return 1
}

usuario_actual (){
    if [ -s "$SESSION_FILE" ]; then
        cat "$SESSION_FILE"
    else
        echo ""
    fi
}

crear_usuario (){
    while true; do
        read -p "Nombre de Usuario:" nombre
        if grep -q "^$nombre:" "$USERS_FILE"; then
            echo "Error: el usuario '$nombre' ya existe. Intente con otro porfavor."
        else
            echo
            read -s -p "Contraseña: " contra
            echo
            read -s -p "Confirmar contraseña: " contra2
            echo
            if [ -z "$contra" ]; then
                echo "La contraseña no puede ser vacía."
            elif [ "$contra" != "$contra2" ]; then
                echo "Las contraseñas no coinciden. Intente de nuevo."
            else
                break
            fi
        fi
    done
    echo "$nombre:$contra" >> "$USERS_FILE"
    echo "Usuario '$nombre' creado con éxito."
}

cambiar_contra (){
    user="$(usuario_actual)"
    if [ -z "$user" ]; then
        echo "Necesitas logearte para cambiar la contraseña."
        return
    fi

    while true; do
        echo "Cambiando la contraseña de '$user'"
        read -s -p "Contraseña actual: " contraAct
        echo

        contraGuardada="$(grep "^$user:" "$USERS_FILE" | cut -d: -f2-)"

        if [ "$contraAct" != "$contraGuardada" ]; then
            echo "Contraseña actual incorrecta."
            return
        fi

        read -s -p "Nueva contraseña: " nuevaContra
        echo
        read -s -p "Confirmar nueva contraseña: " nuevaContra2
        echo

        if [ -z "$nuevaContra" ]; then
            echo "La nueva contraseña no puede ser vacía."
        elif [ "$nuevaContra" != "$nuevaContra2" ]; then
            echo "Las contraseñas no coinciden. Intente otra vez."
        else
            # Reemplazar contraseña en el archivo
            sed -i "s/^$user:.*/$user:$nuevaContra/" "$USERS_FILE"
            echo "Contraseña actualizada con éxito."
            break
        fi
    done
}


login_usuario (){
    if [ -n "$(usuario_actual)" ]; then
        echo "Ya hay un usuario logueado: $(usuario_actual). Haga logout antes."
        return
    fi

    while true; do
        read -p "Usuario: " usuario
        read -s -p "Contraseña: " contra
        echo

        contraGuardada="$(grep "^$usuario:" "$USERS_FILE" | cut -d: -f2-)"

        if [ -z "$contraGuardada" ]; then
            echo "Usuario no encontrado."
            return
        fi

        if [ "$contra" = "$contraGuardada" ]; then
            echo "$usuario" > "$SESSION_FILE"
            echo "BIENVENIDO: $usuario"
            return
        else
            echo "Contraseña incorrecta."
        fi
    done
}


logout_usuario (){
    if [ ! -n "$(usuario_actual)" ]; then
        echo "No hay ningún usuario logeado."
        return
    fi
    echo "" > "$SESSION_FILE"
    echo "Logout exitoso."
}

ingresar_producto () {
    user="$(usuario_actual)"
    if [ -z "$user" ]; then
        echo "Necesitas logearte para ingresar un producto."
        return
    fi

    echo "Ingreso de nuevo producto"
    echo "Tipos válidos: Base, Layer, Shade, Dry, Contrast, Technical, Texture, Mediums"

    read -p "Tipo: " tipo

    case "$tipo" in
        Base|Layer|Shade|Dry|Contrast|Technical|Texture|Mediums)
            ;;
        *)
            echo "Tipo inválido. Operación cancelada."
            return
            ;;
    esac

    read -p "Modelo (nombre): " modelo
    if [ -z "$modelo" ]; then
        echo "Modelo no puede estar vacío."
        return
    fi

    read -p "Descripción breve: " desc

    while true; do
        read -p "Cantidad (entero >=0): " cant
        if [[ "$cant" =~ ^[0-9]+$ ]]; then break
        else echo "Cantidad inválida."; fi
    done

    while true; do
        read -p "Precio unitario (entero): " precio
        if [[ "$precio" =~ ^[0-9]+$ ]]; then break
        else echo "Precio inválido."; fi
    done

    codigo="$(echo "$tipo" | cut -c1-3 | tr '[:lower:]' '[:upper:]')"

    tmpfile="$(mktemp)"
    encontrado=0

    while IFS='|' read -r cod t m d old_cant old_precio; do
        if [ "$t" = "$tipo" ] && [ "$m" = "$modelo" ]; then
            nuevo_cant=$((old_cant + cant))
            echo "$cod|$t|$m|$desc|$nuevo_cant|$precio" >> "$tmpfile"
            encontrado=1
        else
            echo "$cod|$t|$m|$d|$old_cant|$old_precio" >> "$tmpfile"
        fi
    done < "$PRODUCTS_FILE"

    if [ "$encontrado" -eq 0 ]; then
        printf "%s|%s|%s|%s|%s|%s\n" "$codigo" "$tipo" "$modelo" "$desc" "$cant" "$precio" >> "$tmpfile"
        echo "Producto nuevo registrado."
    else
        echo "Producto existente actualizado."
    fi

    mv "$tmpfile" "$PRODUCTS_FILE"

    echo "✅ Estado actual del producto:"
    grep "|$tipo|$modelo|" "$PRODUCTS_FILE"
}

vender_producto() {
    user="$(usuario_actual)"
    if [ -z "$user" ]; then
        echo "Necesitas logearte para vender un producto."
        return
    fi

    echo "Venta de producto"
    read -p "Tipo del producto: " tipo
    read -p "Modelo del producto: " modelo
    read -p "Cantidad a vender: " cantidad

    # Validación cantidad positiva
    if ! [[ "$cantidad" =~ ^[0-9]+$ ]] || [ "$cantidad" -le 0 ]; then
        echo "Cantidad inválida."
        return
    fi

    encontrado=0
    tmpfile="$(mktemp)"

    while IFS='|' read -r codigo t m desc cant precio; do
        if [ "$tipo" = "$t" ] && [ "$modelo" = "$m" ]; then
            encontrado=1

            if [ "$cant" -lt "$cantidad" ]; then
                echo "Stock insuficiente. Disponible: $cant"
                rm "$tmpfile"
                return
            fi

            nuevo_cant=$((cant - cantidad))
            echo "$codigo|$t|$m|$desc|$nuevo_cant|$precio" >> "$tmpfile"
            echo "Venta realizada. Nuevo stock: $nuevo_cant"
        else
            echo "$codigo|$t|$m|$desc|$cant|$precio" >> "$tmpfile"
        fi
    done < "$PRODUCTS_FILE"

    if [ "$encontrado" -eq 0 ]; then
        echo "Producto no encontrado."
        rm "$tmpfile"
        return
    fi

    mv "$tmpfile" "$PRODUCTS_FILE"
}


vender_producto () {
    user="$(usuario_actual)"
    if [ -z "$user" ]; then
        echo "Debes estar logeado para vender productos."
        return
    fi

    if [ ! -s "$PRODUCTS_FILE" ]; then
        echo "No hay productos cargados."
        return
    fi

    # Mostrar lista numerada de productos
    echo "Productos disponibles:"
    mapfile -t productos < "$PRODUCTS_FILE"

    for i in "${!productos[@]}"; do
        IFS='|' read -r cod tipo mod desc cant precio <<< "${productos[$i]}"
        echo "$((i+1))) $tipo - $mod - \$ $precio (Stock: $cant)"
    done

    # Compra acumulada
    resumenTipos=()
    resumenModelos=()
    resumenCantidades=()
    resumenTotales=()

    while true; do
        read -p "Ingrese número de producto a comprar (0 para finalizar): " num
        if [ "$num" = "0" ]; then
            break
        fi

        if ! [[ "$num" =~ ^[0-9]+$ ]] || (( num < 1 || num > ${#productos[@]} )); then
            echo "Número inválido."
            continue
        fi

        index=$((num-1))
        IFS='|' read -r cod tipo mod desc cant precio <<< "${productos[$index]}"

        if [ "$cant" -eq 0 ]; then
            echo "No hay stock disponible para este producto."
            continue
        fi

        read -p "Cantidad a comprar: " compra
        if ! [[ "$compra" =~ ^[0-9]+$ ]] || (( compra < 1 || compra > cant )); then
            echo "Cantidad inválida o insuficiente stock."
            continue
        fi

        # Restar stock
        nuevoCant=$((cant - compra))
        productos[$index]="$cod|$tipo|$mod|$desc|$nuevoCant|$precio"

        total=$((precio * compra))
        resumenTipos+=("$tipo")
        resumenModelos+=("$mod")
        resumenCantidades+=("$compra")
        resumenTotales+=("$total")

        echo "Producto agregado a la compra."
    done

    # Si no se compró nada
    if [ ${#resumenTipos[@]} -eq 0 ]; then
        echo "No se realizó ninguna compra."
        return
    fi

    # Guardar stock actualizado
    printf "%s\n" "${productos[@]}" > "$PRODUCTS_FILE"

    echo
    echo "====== RESUMEN DE COMPRA ======"
    totalFinal=0
    for i in "${!resumenTipos[@]}"; do
        echo "- ${resumenTipos[$i]} | ${resumenModelos[$i]} | Cantidad: ${resumenCantidades[$i]} | Total: \$ ${resumenTotales[$i]}"
        totalFinal=$((totalFinal + resumenTotales[$i]))
    done
    echo "TOTAL A PAGAR: \$ $totalFinal"
    echo "=============================="
}

filtrar_productos () {
    if [ ! -s "$PRODUCTS_FILE" ]; then
        echo "No hay productos en el inventario."
        return
    fi

    echo "Filtrar por tipo (Base, Layer, Shade, Dry, Contrast, Technical, Texture, Mediums)"
    read -p "Dejar vacío para ver todos: " filtro

    echo "------ PRODUCTOS ------"

    if [ -z "$filtro" ]; then
        cat "$PRODUCTS_FILE" | while IFS='|' read cod tipo mod desc cant precio; do
            echo "$tipo - $mod - Cant: $cant - Precio: \$ $precio"
        done
    else
        grep -i "|$filtro|" "$PRODUCTS_FILE" | while IFS='|' read cod tipo mod desc cant precio; do
            echo "$tipo - $mod - Cant: $cant - Precio: \$ $precio"
        done
    fi

    echo "-----------------------"
}

crear_reporteCSV () {
    mkdir -p "$BASE_DIR/Datos"
    REPORTE="$BASE_DIR/Datos/datos.CSV"

    echo "Código,Tipo,Modelo,Descripción,Cantidad,Precio" > "$REPORTE"

    while IFS='|' read cod tipo mod desc cant precio; do
        echo "$cod,$tipo,$mod,$desc,$cant,$precio" >> "$REPORTE"
    done < "$PRODUCTS_FILE"

    echo "Reporte generado en: Datos/datos.CSV ✅"
}

main_menu (){
crear_archivos
while true; do
    echo "----------------------------------------"
    echo "Sistema de Inventario - Citadel (Bash)"
    echo "Usuario actual: $(usuario_actual)"
    echo "1) Crear usuario"
    echo "2) Cambiar contraseña"
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
            2) cambiar_contra ;;
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

main_menu