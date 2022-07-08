'use strict';

// API
let TodolistAPI = (function() {
    function todolistAPI() {}

    // Get list of all groups
    todolistAPI.prototype.getGroupsList = function() {
        return $.get('/api/group/list', {}, 'json');
    };

    // Delete group by name
    todolistAPI.prototype.deleteGroup = function(group) {
        return $.get('/api/group/delete', {
            group: group
        }, 'json');
    };

    // Get all todos
    todolistAPI.prototype.getAllTodos = function() {
        return $.get('/api/todos/all', {}, 'json');
    };

    // Get todos by group name
    todolistAPI.prototype.getTodosByGroup = function(group) {
        return $.get('/api/todos', {
            group: group
        }, 'json');
    };

    // Change todo status
    todolistAPI.prototype.changeTodoStatus = function(group, id, status) {
        return $.get('/api/todos/status/change', {
            group: group,
            todoid: id,
            status: status
        }, 'json');
    };

    // Create new todo in group
    todolistAPI.prototype.createNewTodo = function(group, text) {
        return $.get('/api/todos/add', {
            group: group,
            text: text,
        }, 'json');
    };

    // Delete todo item
    todolistAPI.prototype.deleteTodo = function(group, todoid) {
        return $.get('/api/todos/delete', {
            group: group,
            todoid: todoid,
        }, 'json');
    }

    return todolistAPI;
})();

// UI events
let TodolistUI = (function() {
    let $this;

    function todolistUI() {
        $this = this;
    }

    // Generate html for todo element
    todolistUI.prototype.generateTaskElement = function(group, id, status, date, text) {
        return `
            <div class="task-element" group="${group}" status="${status}" todo-id="${id}">
                <div class="task-element-text">
                    <div class="task-element-text-info">${date} <div class="task-element-id">#${group}${id}</div></div>
                    <div class="task-element-text-todo">${text}</div>
                </div>
                <div class="task-element-delete">
                    <img src="./static/images/delete.svg">
                </div>
            </div>
        `;
    };

    // Set white background for all groups
    todolistUI.prototype.unselectAllGroup = function() {
        $(".todolist-groups-wrapper >").each(function() {
            $(this).css("background", "#fff")
        });
    };

    // Get and draw todos for group
    todolistUI.prototype.drawTodos = function(group) {
        // Clear columns
        $(".todolist-todo-column-body").html("");
        $(".todolist-doing-column-body").html("");
        $(".todolist-done-column-body").html("");

        if (group == "all") {
            api.getAllTodos().done((data) => {
                for (let [group, todos] of Object.entries(data)) {
                    if (todos.length > 0) {
                        for (let todo of todos.reverse()) {
                            $(`.todolist-${todo.STATUS}-column-body`).append($this.generateTaskElement(
                                group.toLowerCase(), todo.ID, todo.STATUS, todo.DATE, todo.TEXT
                            ));
                        }
                    }
                }
            })
        } else {
            api.getTodosByGroup(group).done((data) => {
                for (let todo of data.reverse()) {
                    $(`.todolist-${todo.STATUS}-column-body`).append($this.generateTaskElement(
                        group, todo.ID, todo.STATUS, todo.DATE, todo.TEXT
                    ));
                }
            });
        }
    };

    // Select group
    todolistUI.prototype.selectGroup = function(group) {
        $this.unselectAllGroup();
        $(`#group-${group}`).css("background", "rgba(0, 0, 0, 0.1)");
        $this.drawTodos(group);
    };

    // Select group event handler
    todolistUI.prototype.selectGroupClick = function(event) {
        let groupElement = $(event.target).closest(".todolist-group-button");
        let group = $(groupElement).attr("id").replace("group-", "");
        $this.selectGroup(group);
    };

    // Delete group event handler
    todolistUI.prototype.deleteGroup = function() {
        let groupElement = $(event.target).closest(".todolist-group-button");
        let group = $(groupElement).attr("id").replace("group-", "");

        api.deleteGroup(group).done((data) => {
            if (data.error == undefined) {
                $(`#group-${group}`).remove();
                $("#group-all span").click();
            } else {
                alert(data.error);
            }
        });
    };

    // Delele todo event handler
    todolistUI.prototype.deleteTodo = function(event) {
        let todoElement = $(event.target).closest(".task-element");
        let group = $(todoElement).attr("group");
        let id = $(todoElement).attr("todo-id");

        api.deleteTodo(group, id).done((data) => {
            if (data.error == undefined) {
                $(`.task-element[todo-id="${data.ID}"][group="${data.GROUP}"]`).remove();
            } else {
                alert(data.error);
            }
        });
    };

    // Create new todo
    todolistUI.prototype.createTodo = function(event) {
        let taskText = $("#task-input").val();
        let taskGroup = $("#task-group").val().replaceAll(" ", "_");

        if (taskText == "" || taskGroup == "") {
            alert("The task or its group cannot be empty");
            return;
        }

        api.createNewTodo(taskGroup, taskText).done((data) => {
            if (data.error == undefined) {
                $(".todolist-todo-column-body").prepend($this.generateTaskElement(
                    data.GROUP, data.ID, data.STATUS, data.DATE, data.TEXT
                ));

                if ($(`#group-${taskGroup}`).length == 0) {
                    // Append new group
                    $(".todolist-groups-wrapper").append(`
                        <div class="todolist-group-button" id="group-${taskGroup}" style="background: rgba(255, 255, 255);">
                            <span>${taskGroup.replaceAll("_", " ")}</span>
                            <div class="group-element-delete">
                                <img src="./static/images/delete.svg">
                            </div>
                        </div>
                    `);

                    $(`#group-${taskGroup} span`).click(ui.selectGroupClick);
                    $(`#group-${taskGroup} .group-element-delete`).click(ui.deleteGroup);
                    $(`#group-${taskGroup} span`).click();
                }

                $(`.task-element[todo-id="${data.ID}"][group="${taskGroup}"] .task-element-delete`).click(ui.deleteTodo);
                $("#task-input").val("");
                $("#task-input").removeAttr("style");
                $("#task-group").val("");
            } else {
                alert(data.error);
            }
        });
    };

    // Search event handler
    todolistUI.prototype.searchTodo = function() {
        let filter = $("#search-input").val().toLowerCase(),
            todos = $(".task-element");

        for (let i = 0; i < todos.length; i++) {
            let element = todos[i];
            let text = $(element).text().toLowerCase();
            $(element).css("display", text.includes(filter) ? "flex" : "none");
        }
    };

    // Dragable event handler
    todolistUI.prototype.dragable = function(event, ui) {
        let element = $(event.originalEvent.target).closest(".task-element");
        let newStatus = $(event.target)
            .attr("class")
            .match(/todolist-(.*)-column-body/i)[1];

        api.changeTodoStatus(
            element.attr("group"),
            element.attr("todo-id"),
            newStatus
        ).done((data) => {
            if (data.error == undefined) {
                // Change status on element
                $(element).attr("status", newStatus);
            } else {
                alert(data.error);
            }
        });
    };

    return todolistUI;
})();

let api = new TodolistAPI();
let ui = new TodolistUI();

let main = () => {
    api.getGroupsList().done((data) => {
        for (let i of data) {
            // Draw all groups button
            $(".todolist-groups-wrapper")
                .append(`
                    <div class="todolist-group-button" id="group-${i.NAME.toLowerCase()}">
                        <span>${i.NAME.replaceAll("_", " ")}</span>
                        <div class="group-element-delete">
                            <img src="./static/images/delete.svg">    
                        </div>
                    </div>
                `);
        }

        ui.selectGroup("all");

        // Enable dragabling
        $(".column").sortable({
            connectWith: ".column",
            update: ui.dragable,
            revert: true,
            handle: ".task-element-text-info"
        });

        // Setup event handlers
        $(".todolist-group-button span").click(ui.selectGroupClick);
        $(".group-element-delete").click(ui.deleteGroup);
        $(".todolist-body").on("click", ".task-element .task-element-delete", ui.deleteTodo);
        $(".send-task-button").click(ui.createTodo);
        $("#search-input").keyup(ui.searchTodo);
    });
};

$(document).ready(main);
