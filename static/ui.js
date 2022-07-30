/**
 * UI events
 */
export class TodolistUI {
    constructor (api) {
        this.api = api;

        // Enable dragabling
        $(".column").sortable({
            connectWith: ".column",
            update: (event) => this.dragable(event),
            revert: true,
            handle: ".task-element-text-info"
        });
    };

    /**
     * Init UI
     */
    init () {
        this.api.getGroupsList().done((data) => {
            data.forEach(i => {
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
            });

            this.selectGroup("all");
    
            // Setup event handlers
            $(".todolist-group-button span")
                .click((event) => this.selectGroupClick(event));
            $(".group-element-delete")
                .click((event) => this.deleteGroup(event));
            $(".todolist-body")
                .on("click", ".task-element .task-element-delete img", 
                    (event) => this.deleteTodo(event));
            $(".send-task-button")
                .click((event) => this.createTodo(event));
            $("#search-input")
                .keyup((event) => this.searchTodo(event));
        });
    };

    /**
     * Generate html for todo element
     * 
     * @param {string} group 
     * @param {int} id 
     * @param {string} status 
     * @param {string} date 
     * @param {string} text 
     * @returns string
     */
    generateTaskElement (group, id, status, date, text) {
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

    /**
     * Set white background for all groups
     */
    unselectAllGroup () {
        $(".todolist-groups-wrapper >").each(function() {
            $(this).css("background", "#fff")
        });
    };

    /**
     * Get and draw todos for group
     * 
     * @param {string} group 
     */
    drawTodos (group) {
        // Clear columns
        $(".todolist-todo-column-body").html("");
        $(".todolist-doing-column-body").html("");
        $(".todolist-done-column-body").html("");

        if (group == "all") {
            this.api.getAllTodos().done((data) => {
                for (let [group, todos] of Object.entries(data)) {
                    todos.reverse().forEach((todo) => {
                        $(`.todolist-${todo.STATUS}-column-body`)
                            .append(this.generateTaskElement(
                                group.toLowerCase(), todo.ID, todo.STATUS, todo.DATE, todo.TEXT
                            ));
                    });
                }
            })
        } else {
            this.api.getTodosByGroup(group).done((data) => {
                data.reverse().forEach((todo) => {
                    $(`.todolist-${todo.STATUS}-column-body`)
                        .append(this.generateTaskElement(
                            group, todo.ID, todo.STATUS, todo.DATE, todo.TEXT
                        ));
                });
            });
        }
    };

    /**
     * Select group
     * 
     * @param {string} group 
     */
    selectGroup (group) {
        this.unselectAllGroup();
        $(`#group-${group}`).css("background", "rgba(0, 0, 0, 0.1)");
        this.drawTodos(group);
    };

    /**
     * Select group event handler
     * 
     * @param {Event} event 
     */
    selectGroupClick (event) {
        const groupElement = $(event.target).closest(".todolist-group-button");
        const group = $(groupElement).attr("id").replace("group-", "");
        this.selectGroup(group);
    };

    /**
     * Delete group event handler
     * 
     * @param {Event} event 
     */
    deleteGroup (event) {
        const groupElement = $(event.target).closest(".todolist-group-button");
        const group = $(groupElement).attr("id").replace("group-", "");

        this.api.deleteGroup(group).done((data) => {
            if (data.error == undefined) {
                $(`#group-${group}`).remove();
                $("#group-all span").click();
            } else {
                alert(data.error);
            }
        });
    };

    /**
     * Delele todo event handler
     * 
     * @param {Event} event 
     */
    deleteTodo (event) {
        const todoElement = $(event.target).closest(".task-element");
        const group = $(todoElement).attr("group");
        const id = $(todoElement).attr("todo-id");

        this.api.deleteTodo(group, id).done((data) => {
            if (data.error == undefined) {
                $(`.task-element[todo-id="${data.ID}"][group="${data.GROUP}"]`).remove();
            } else {
                alert(data.error);
            }
        });
    };

    /**
     * Create new todo
     * 
     * @param {Event} event 
     */
    createTodo (event) {
        const taskText = $("#task-input").val();
        const taskGroup = $("#task-group").val().replaceAll(" ", "_");

        if (taskText == "" || taskGroup == "") {
            alert("The task or its group cannot be empty");
            return;
        }

        this.api.createNewTodo(taskGroup, taskText).done((data) => {
            if (data.error == undefined) {
                $(".todolist-todo-column-body")
                    .prepend(this.generateTaskElement(
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

                    $(`#group-${taskGroup} span`)
                        .click((event) => this.selectGroupClick(event));
                    $(`#group-${taskGroup} .group-element-delete`)
                        .click((event) => this.deleteGroup(event));
                    $(`#group-${taskGroup} span`).click();
                }

                $(`.task-element[todo-id="${data.ID}"][group="${taskGroup}"] .task-element-delete img`)
                    .click((event) => this.deleteTodo(event));
                $("#task-input").val("");
                $("#task-group").val("");
            } else {
                alert(data.error);
            }
        });
    };

    /**
     * Search event handler
     */
    searchTodo () {
        const filter = $("#search-input").val().toLowerCase();

        $(".task-element").each((i, element) => {
            const text = $(element).text().toLowerCase();
            $(element).css("display", text.includes(filter) ? "flex" : "none");
        });
    };

    // Dragable event handler
    dragable (event) {
        const element = $(event.originalEvent.target)
            .closest(".task-element");
        const newStatus = $(event.target)
            .attr("class")
            .match(/todolist-(.*)-column-body/i)[1];

        this.api.changeTodoStatus(
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
}