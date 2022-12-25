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
                    .append(this.generateGroupElement(i.NAME.toLowerCase()));
            });

            this.selectGroup("all");
    
            // Select group event
            $(".todolist-wrapper")
                .on("click", ".todolist-group-button span", 
                    (event) => this.selectGroupClick(event));

            // Delete group event
            $(".todolist-wrapper")
                .on("click", ".group-element-delete", 
                    (event) => this.deleteGroup(event));

            // Delete todo event
            $(".todolist-body")
                .on("click", ".task-element .task-element-delete img", 
                    (event) => this.deleteTodo(event));

            // Edit todo event
            $(".todolist-body")
                .on("click", ".task-element .task-element-edit img", 
                    (event) => this.editTodo(event));

            // Create new task event
            $(".todolist-create-button")
                .click(() => this.showCreateTask());

            // Search event
            $("#search-input")
                .keyup((event) => this.searchTodo(event));

            // Open statistics event
            $("#statistics")
                .click(() => this.showStatistics());
        });
    };

    /**
     * Show create task modal window
     */
    showCreateTask() {
        // Remove old toolbar
        $(".todolist-create-task-modal .ql-toolbar").remove();

        // Setup quill editor
        const quill = new Quill(".todolist-create-task-modal .todolist-task-editor", {
            theme: 'snow'
        });  

        // Create todo event
        $(".todolist-create-task-modal")
            .off("click", ".send-task-button")
            .on("click", ".send-task-button", () => {
                this.createTodo(quill)
            });

        // Show modal window
        $(".todolist-create-task-modal").dialog({
            height: window.innerHeight / 1.2,
            width: window.innerWidth / 2,
            modal: true,
            close: function( event, ui ) {
                // Clear inputs
                $("#task-group").val("");
                quill.root.innerHTML = "";
            }
        });
    }

    /**
     * Create new todo
     *  
     * @param {Quill} quill 
     */
    createTodo (quill) {
        const taskText = quill.root.innerHTML;
        const taskGroup = $("#task-group").val().replaceAll(" ", "_");

        if (taskText == "" || taskGroup == "") {
            alert("The task or its group cannot be empty");
            return;
        }

        this.api.createNewTodo(taskGroup, taskText).done((data) => {
            if (data.ERROR == undefined) {
                $(".todolist-todo-column-body")
                    .prepend(this.generateTaskElement(
                        data.GROUP, data.ID, data.STATUS, data.DATE, data.TEXT
                    ));

                if ($(`#group-${taskGroup}`).length == 0) {
                    // Append new group
                    $(".todolist-groups-wrapper").append(this.generateGroupElement(taskGroup));
                    $(`#group-${taskGroup} span`).click();
                }

                // Close modal
                $(".todolist-create-task-modal").dialog("close")
            } else {
                alert(data.ERROR);
            }
        });
    };

    /**
     * Show statistics modal window
     */
    showStatistics () {
        this.api.getTodosStats().done((data) => {
            let content = "";
            let lineData = [];

            // Setup data
            Object.entries(data).forEach((group) => {
                content += `<div class="statistic-element" data-group="${group[1].ORIGNAME}"></div>`;
                lineData.push({
                    title: group[1].ORIGNAME,
                    items: [
                        { name: "TODO",  value: group[1].TODO,  color: "#999999" }, 
                        { name: "DOING", value: group[1].DOING, color: "#555555" },
                        { name: "DONE",  value: group[1].DONE,  color: "#222222" },
                    ]
                });
            });

            // Show modal window
            $(".todolist-statistics-modal").dialog({
                height: window.innerHeight / 1.2,
                width: window.innerWidth / 2,
                modal: true,
                open: function( event, ui ) {
                    $(event.target).html(content);

                    // Setup liner bars
                    lineData.forEach((element) => {
                        (new LinerBar(
                            `.statistic-element[data-group="${element.title}"]`, 
                            element
                        )).render();
                    });
                }
            });
        })
    };

    /**
     * Generate html for todo element
     * 
     * @param   {string} group 
     * @param   {int}    id 
     * @param   {string} status 
     * @param   {string} date 
     * @param   {string} text 
     * @returns {string}
     */
    generateTaskElement (group, id, status, date, text) {
        // Remove html tags from text
        text = $("<div>").html(text).text();

        // Return template
        return `
            <div class="task-element" group="${group}" status="${status}" todo-id="${id}">
                <div class="task-element-text">
                    <div class="task-element-text-info">${date} <div class="task-element-id">#${group}${id}</div></div>
                    <div class="task-element-text-todo">${text}</div>
                </div>
                <div class="task-element-options">
                    <div class="task-element-delete">
                        <img src="./static/images/delete.svg">
                    </div>
                    <div class="task-element-edit">
                        <img src="./static/images/pencil.svg">
                    </div>
                </div>
            </div>
        `;
    };

    /**
     * Generate html for group button 
     * 
     * @param   {string} taskGroup 
     * @returns {string}
     */
    generateGroupElement (taskGroup) {
        return `
            <div class="todolist-group-button" id="group-${taskGroup}" style="background: rgba(255, 255, 255);">
                <span>${taskGroup.replaceAll("_", " ")}</span>
                <div class="group-element-delete">
                    <img src="./static/images/delete.svg">
                </div>
            </div>
        `
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
            if (data.ERROR == undefined) {
                $(`#group-${group}`).remove();
                $("#group-all span").click();
            } else {
                alert(data.ERROR);
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
            if (data.ERROR == undefined) {
                $(`.task-element[todo-id="${data.ID}"][group="${data.GROUP}"]`).remove();
            } else {
                alert(data.ERROR);
            }
        });
    };

    /**
     * Open edit window for todo
     * 
     * @param {Event} event 
     */
    editTodo (event) {
        const todoElement = $(event.target).closest(".task-element");
        const group = $(todoElement).attr("group");
        const id = $(todoElement).attr("todo-id");

        this.api.getTodoByGroupAndId(group, id).done((data) => {
            if (data.ERROR == undefined) {
                // Remove old toolbar
                $(".todolist-edit-task-modal .ql-toolbar").remove();

                // Setup quill editor
                const quill = new Quill(".todolist-edit-task-modal .todolist-task-editor", {
                    theme: 'snow'
                }); 
                quill.root.innerHTML = data.TEXT;

                // Show modal window
                $(".todolist-edit-task-modal").dialog({
                    height: window.innerHeight / 1.2,
                    width: window.innerWidth / 2,
                    modal: true,
                    close: function( event, ui ) {
                        // Clear inputs
                        $("#task-group").val("");
                        quill.root.innerHTML = "";
                    }
                });
            } else {
                alert(data.ERROR);
            }
        });
    }

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
            if (data.ERROR == undefined) {
                // Change status on element
                $(element).attr("status", newStatus);
            } else {
                alert(data.ERROR);
            }
        });
    };
}