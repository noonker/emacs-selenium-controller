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

"""
<span style="position: absolute; display: block; top: -1px; left: -1px; white-space: nowrap; overflow: hidden; font-size: 9px; padding: 1px 3px 0px 3px; background: linear-gradient(to bottom, #FF1493 0%,#FF69B4 100%); border: solid 1px #C38A22; border-radius: 3px; box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3); color: white;">AB</span>
"""
