{
 "cells": [
  {
   "cell_type": "code",
   "execution_count": null,
   "id": "4c8ec6e7",
   "metadata": {},
   "outputs": [],
   "source": [
    "def acs_read(dat_path):\n",
    "    \n",
    "    with open (dat_path) as f:\n",
    "               lines = f.readlines()\n",
    "               lines.pop(0)\n",
    "               lines.pop(0)\n",
    "               x = [line.split()[0] for line in lines]\n",
    "               y = [line.split()[1] for line in lines]\n",
    "\n",
    "    x_arr = []\n",
    "    y_arr = []\n",
    "\n",
    "    for item in x:\n",
    "        x_arr.append(float(item))\n",
    "\n",
    "    for item in y:\n",
    "        y_arr.append(float(item))\n",
    "    \n",
    "    xn = np.array(x_arr)\n",
    "    yn = np.array(y_arr)\n",
    "    \n",
    "    return [xn, yn]"
   ]
  },
  {
   "cell_type": "markdown",
   "id": "e285a9c5",
   "metadata": {},
   "source": [
    "#### ghghghghghghggh"
   ]
  }
 ],
 "metadata": {
  "kernelspec": {
   "display_name": "Python 3",
   "language": "python",
   "name": "python3"
  },
  "language_info": {
   "codemirror_mode": {
    "name": "ipython",
    "version": 3
   },
   "file_extension": ".py",
   "mimetype": "text/x-python",
   "name": "python",
   "nbconvert_exporter": "python",
   "pygments_lexer": "ipython3",
   "version": "3.8.8"
  }
 },
 "nbformat": 4,
 "nbformat_minor": 5
}
