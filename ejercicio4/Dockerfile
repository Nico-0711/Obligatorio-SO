FROM node:18-alpine

WORKDIR /usr/src/app

# Copiar dependencias desde /web
COPY web/package*.json ./

RUN npm install --production

# Copiar todo el código de la app
COPY web/ .

EXPOSE 5000

CMD ["node", "server.js"]
