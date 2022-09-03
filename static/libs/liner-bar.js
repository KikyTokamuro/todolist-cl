// MIT License

// Copyright (c) 2022 Daniil Arkhangelsky (KikyTokamuro)

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

const LinerBar = (function() {
    let $this;

    const errParams = "ElementSelector or Data is empty";
    const errSelector = "Elements not found";

    /**
     * LinerBar controler
     *
     * @param  {string} elementSelector
     * @param  {array}  data
     * @return {void}
     */
    function linerBar(elementSelector = "", data = []) {
        $this = this;
        $this.elementSelector = elementSelector;
        $this.data = data;
    }

    /**
     * Render LinerBar
     *
     * @return {object | void}
     */
    linerBar.prototype.render = function() {
        // Check arguments
        if ($this.elementSelector == "" || $this.data.length == 0) {
            return {
                error: errParams
            };
        }

        // Search elements by selector
        $this.linerBarElements = document.querySelectorAll($this.elementSelector);
        if ($this.linerBarElements.length == 0) {
            return {
                error: errSelector
            };
        }

        // Prepare elements
        $this.linerBarElements.forEach((element, elementIndex) => {
            let title, bar, legend;

            // Create title
            if ($this.data.title) {
                title = createElementWithClass("p", "liner-bar-card-title"
					       + ($this.data.dark ? " liner-bar-dark" : ""));
                title.textContent = $this.data.title;
            }

            // Create progress bar
            bar = createElementWithClass("div", "liner-bar-progess");
            // Create legend
            legend = createElementWithClass("div", "liner-bar-legend");

            // Calculate sum of values
            const totalValues = $this.data.items
                .map((item) => item.value)
                .reduce((a, b) => a + b, 0);

            // Processing data items
            $this.data.items.forEach((item, itemIndex) => {
                // Creat progress item
                let progressItem = createElementWithClass("div", "liner-bar-progess-item");
                progressItem.setAttribute("data-id", `${elementIndex}-${itemIndex}`);
                progressItem.textContent = item.value;
                progressItem.style.width = `${(item.value / totalValues) * 100}%`;
                progressItem.style.backgroundColor = item.color;
                bar.appendChild(progressItem);

                // Create legend dot
                let legendItemDot = createElementWithClass("span", "liner-bar-legend-item-dot");
                legendItemDot.style.backgroundColor = item.color;

                // Create legend name
                let legendItemName = createElementWithClass("span", "liner-bar-legend-item-name"
							    + ($this.data.dark ? " liner-bar-dark" : ""));
                legendItemName.textContent = item.name;

                // Create legend button
                let legendItemBtn = createElementWithClass("button", "liner-bar-legend-item-button");
                legendItemBtn.setAttribute("data-target", `${elementIndex}-${itemIndex}`);
                legendItemBtn.appendChild(legendItemDot);
                legendItemBtn.appendChild(legendItemName);

                // Create legend item
                let legendItem = createElementWithClass("div", "liner-bar-legend-item");
                legendItem.appendChild(legendItemBtn);

                // Add legend item to legend
                legend.appendChild(legendItem);
            });

            // Create bar card body
            let barCardBody = createElementWithClass("div", "liner-bar-card-body");
            if ($this.data.title) {
                barCardBody.appendChild(title);
            }
            barCardBody.appendChild(bar);
            barCardBody.appendChild(legend);

            // Create bar card
            let barCard = createElementWithClass("div", "liner-bar-card"
						 + ($this.data.dark ? " liner-bar-dark" : ""));
            barCard.appendChild(barCardBody);

            // Get html from bar card
            let tmp = createElementWithClass("div");
            tmp.appendChild(barCard);
            element.innerHTML = tmp.innerHTML;

            // Add mouse events
            element.querySelectorAll(".liner-bar-progess-item")
                .forEach((barProgressItem) => {
                    ["mouseenter", "mouseleave"].forEach((eventType) => {
                        barProgressItem.addEventListener(eventType, (event) => {
                            activateElement(event, barProgressItem);
                        });
                    });
                });

            // Add click events
            element.querySelectorAll(`.liner-bar-legend-item-button`)
                .forEach((legendButton) => {
                    legendButton.addEventListener("click", () => {
                        const dataId = legendButton.getAttribute("data-target");
                        const target = element.querySelector(`.liner-bar-progess-item[data-id='${dataId}']`);

                        if (target.classList.contains("activated")) {
                            target.classList.remove("activated");
                        } else {
                            element.querySelectorAll(".liner-bar-progess")
                                .forEach((progressElement) => {
                                    progressElement.classList.remove("activated");
                                });

                            target.classList.add("activated");
                        }
                    });
                });
        });
    };

    /**
     * Clear LinerBar elements
     */
    linerBar.prototype.clear = function() {
        $this.linerBarElements
            .forEach((element) =>
                element.removeChild(element.querySelector(".liner-bar-card")));
    };

    /**
     * Activate progress bar item
     *
     * @param  {Event}  event
     * @param  {object} element 
     * @return {void}
     */
    const activateElement = (event, element) => {
        if (event.type === "mouseenter") {
            if (!element.classList.contains("activated")) {
                element.classList.add("activated");
            }
        } else if (event.type === "mouseleave") {
            if (element.classList.contains("activated")) {
                element.classList.remove("activated");
            }
        }
    }

    /**
     * Create DOM element with class
     *
     * @param  {string} classname
     * @param  {string} elementType
     * @return {object}
     */
    const createElementWithClass = (elementType, className = "") => {
        const newElement = document.createElement(elementType);
        if (className) {
            className.split(" ").forEach(function(value) {
                newElement.classList.add(value);
            });
        }
        return newElement;
    }

    return linerBar;
})();
