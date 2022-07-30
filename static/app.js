import { TodolistAPI } from "./api.js";
import { TodolistUI }  from "./ui.js";

const api = new TodolistAPI();
const ui = new TodolistUI(api);
ui.init();