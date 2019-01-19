import string

def apply_style(driver, element):
    driver.execute_script("arguments[0].setAttribute('style', arguments[1]);",
                          element, "background: yellow; border: 2px solid red;")
    original_style = element.get_attribute('style')


def highlight(element):
    """Highlights (blinks) a Selenium Webdriver element"""
    driver = element._parent
    def apply_style(s):
        driver.execute_script("arguments[0].setAttribute('style', arguments[1]);",
                              element, s)
    original_style = element.get_attribute('style')
    apply_style("background: yellow; border: 2px solid red;")

def generate_tags(number):
    letters = list(string.ascii_lowercase)
    letters.extend([i+b for i in letters for b in letters])
    return letters[:number]

div = """var btn = document.createElement("DIV");
var t = document.createTextNode("");
btn.appendChild(t);
btn.id = "emacsControllerClickables"
"""

elem = """var span = document.createElement("SPAN")
var t = document.createTextNode("{}")
span.appendChild(t)
span.style.cssText = "position: absolute; display: block; top: {}px; left: {}px; white-space: nowrap; overflow: hidden; font-size: 12px; padding: 1px 3px 0px 3px; background: linear-gradient(to bottom, #FF1493 0%,#FF69B4 100%); border: solid 1px #C38A22; border-radius: 3px; box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3); color: white; z-index: 10000"
btn.appendChild(span)
"""

def create_markers(controller):    
    script = div
    mapping = {}
    clickables = controller.find_elements_by_tag_name("a")
    clickables = clickables + controller.find_elements_by_tag_name("button")
    clickables = clickables + controller.find_elements_by_tag_name("input")
    tags = generate_tags(len(clickables))
    for element in clickables:
        tag_name = tags.pop()
        mapping[tag_name] = element
        script = script + elem.format(
            tag_name,
            element.location["y"],
            element.location["x"])
    script = script + "document.body.appendChild(btn);"
    controller.execute_script(script)
    return mapping

def select_marker(tag, markers):
    return markers[tag]
    
def close_markers(controller):
    controller.execute_script("""var element = document.getElementById("emacsControllerClickables");
    element.parentNode.removeChild(element);""")
    return True

def switch_window(controller):
    try:
        current_window = controller.current_window_handle
    except:
        current_window = controller.window_handles[0]
    titles = {}
    for window in controller.window_handles:
        controller.switch_to.window(window)
        titles[window] = controller.title
    controller.switch_to.window(current_window)
    for title in titles.values():
        print(title)
    return titles
    
def switch_window_switch(value, windows):
    for window in windows:
        if windows[window] == value:
            selected = window
    controller.switch_to_window(selected)
