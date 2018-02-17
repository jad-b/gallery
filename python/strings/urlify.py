from array import array


def urlify(s, truelength=None):
    """Replace the spaces in a string with the url-encoded version '%20'.

    Accomplished in two passes: First, the string is converted into an array,
    and the spaces are counted. Second, working backwards, the array characters
    are shifted to accomodate the expansion of ' ' => '%20'.

    Example:

        >>> urlify("Mr John Smith  ", 13)
        "Mr%20John%20Smith"
    """
    if truelength is None:
        truelength = len(s)
    spaces = 0
    for i in range(truelength):
        if s[i] == ' ':
            spaces += 1

    arr = array('u', s)  # Create unicode array from string
    for i in range(truelength-1, -1, -1):
        if arr[i] == ' ':
            spaces -= 1
            arr[i], arr[i+1], arr[i+2] = '%', '2', '0'
        else:
            # Shift character by the expanded number of spaces
            arr[i+2*spaces] = arr[i]
    assert i == 0 and spaces == 0
    return arr.tounicode()  # Convert back to string
