/**
 * API methods
 */
export class TodolistAPI {
    constructor () {}

    /**
     * Get list of all groups
     * 
     * @returns jqXHR
     */
    getGroupsList () {
        return $.get('/api/group/list', {}, 'json');
    }

    /**
     * Delete group by name
     * 
     * @param {string} group
     * @returns jqXHR
     */
    deleteGroup (group) {
        return $.post('/api/group/delete', {
            group: group
        }, 'json');
    }

    /**
     * Get all todos
     * 
     * @returns jqXHR
     */
    getAllTodos () {
        return $.get('/api/todos/all', {}, 'json');
    }

    /**
     * Get todos by group name
     * 
     * @param {string} group 
     * @returns jqXHR
     */
    getTodosByGroup (group) {
        return $.get('/api/todos', {
            group: group
        }, 'json');
    }

    /**
     * Get todo by group and id
     * 
     * @param {string} group 
     * @param {number} todoid 
     * @returns jqXHR
     */
    getTodoByGroupAndId(group, todoid) {
        return $.get('/api/todos/get', {
            group:  group,
            todoid: todoid,
        }, 'json');
    }

    /**
     * Get todos statistics
     * 
     * @returns jqXHR
     */
    getTodosStats () {
	    return $.get('/api/todos/stats', {});
    }
    
    /**
     * Change todo status
     * 
     * @param {string} group 
     * @param {number} id 
     * @param {string} status 
     * @returns jqXHR
     */
    changeTodoStatus (group, id, status) {
        return $.post('/api/todos/status/change', {
            group:  group,
            todoid: id,
            status: status
        }, 'json');
    };

    /**
     * Change todo text
     * 
     * @param {string} group 
     * @param {number} id 
     * @param {string} text 
     * @returns jqXHR
     */
     changeTodoText (group, id, text) {
        return $.post('/api/todos/text/change', {
            group:  group,
            todoid: id,
            text:   text
        }, 'json');
    };

    /**
     * Create new todo in group
     * 
     * @param {string} group 
     * @param {string} text 
     * @returns jqXHR
     */
    createNewTodo (group, text) {
        return $.post('/api/todos/add', {
            group: group,
            text:  text,
        }, 'json');
    };

    /**
     * Delete todo item
     * 
     * @param {string} group 
     * @param {number} todoid 
     * @returns jqXHR
     */
    deleteTodo (group, todoid) {
        return $.post('/api/todos/delete', {
            group:  group,
            todoid: todoid,
        }, 'json');
    }
}
