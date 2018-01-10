import cv2
import numpy as np
import time

from sklearn.cluster import dbscan

# TODO: Napravi CNN model za paricki i koristi go nego za klasifikacija kaj broenje (Caffe)


def hist_distance_chisquare(h1, h2):
    h1 /= sum(h1)
    h2 /= sum(h2)

    d = 0

    for i in range(0, len(h1)):
        if h1[i] != 0 and h2[i] != 0:
            d += (h1[i] - h2[i]) ** 2 / (h1[i] + h2[i])

    try:
        return 0.5 * d[0]
    except:
        return 0.5 * d

coin1   = cv2.imread("1den.jpg",  cv2.IMREAD_GRAYSCALE)
coin2   = cv2.imread("2den.jpg",  cv2.IMREAD_GRAYSCALE)
coin5   = cv2.imread("5den.jpg",  cv2.IMREAD_GRAYSCALE)
coin10  = cv2.imread("10den.jpg", cv2.IMREAD_GRAYSCALE)

coin_hist_dict = {
    1:  cv2.calcHist([coin1],  [0], None, [256], [0, 256]),
    2:  cv2.calcHist([coin2],  [0], None, [256], [0, 256]),
    5:  cv2.calcHist([coin5],  [0], None, [256], [0, 256]),
    10: cv2.calcHist([coin10], [0], None, [256], [0, 256]),
}

coins = cv2.imread("mak_coins.jpg")

coins_gray = cv2.cvtColor(coins, cv2.COLOR_BGR2GRAY)


def get_superpixels_labels(coins_lab):
    (w, h) = coins_lab.shape[:2]

    seeds = cv2.ximgproc.createSuperpixelSEEDS(h, w, 3, 200, 10, double_step=False)

    # Glaven faktor za brzina e brojot na iteracii tuka
    seeds.iterate(coins_lab, 50)

    mask = seeds.getLabels()

    mask = mask.astype(np.uint8)

    N = seeds.getNumberOfSuperpixels()

    # Only for debug info
    cnt = seeds.getLabelContourMask()
    cnt = cnt.astype(np.uint8)

    coins_sp = coins.copy()

    for x in range(0, cnt.shape[0]):
        for y in range(0, cnt.shape[1]):
            if cnt[x, y] == 255:
                coins_sp[x, y] = (0, 0, 255)

    print ("There are {} superpixels".format(N))

    cv2.imshow("Coins SP", coins_sp)

    return mask, N


def get_dbscan_clusters_mask(mask, N, coins_hsv):
    # Generate data points for clustering
    data_points = np.empty(shape=(N, 3))
    for i in range(0, N):
        local_mask = np.where(mask == i, 255, 0)
        local_mask = local_mask.astype(np.uint8)

        (m1, m2, m3, _) = cv2.mean(coins_hsv, local_mask)  # hsv dava podobri rezultati od lab

        data_points[i] = [m1, m2, m3]

    core_samples, labels = dbscan(data_points, 16.5, 1)

    for x in range(0, mask.shape[0]):
        for y in range(0, mask.shape[1]):
            label = labels[mask[x, y]]

            if label == -1: # noise is background
                label = 0

            mask[x, y] = label

    mask = np.where(mask == 0, 0, 255)

    mask = mask.astype(np.uint8)

    print ("There are {} clusters".format(np.max (labels)))

    return mask


def count_from_mask(mask):
    coins_circles = coins.copy()

    _, cnts, _ = cv2.findContours(mask.copy(), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    coin_count = 0

    for c in cnts:
        ((x, y), r) = cv2.minEnclosingCircle(c)

        (x, y, r) = (int(x), int(y), int(r))

        local_mask = np.zeros(mask.shape, dtype=np.uint8)

        cv2.circle(coins_circles, (x, y), r, (0, 0, 255))

        cv2.circle(local_mask, (x, y), r, 255, -1)

        hist = cv2.calcHist([coins_gray], [0], local_mask, [256], [0, 256])

        md, mi = (100, -1)

        # Mixed feelings. Machine learning seems necessary. dve destki gi gaga kako 2-ki, drugite ok
        for i in [1, 2, 5, 10]:
            stored_coin_hist = coin_hist_dict[i]
            d = hist_distance_chisquare(stored_coin_hist, hist)

            if d <= md:
                md = d
                mi = i

        cv2.putText(coins_circles, "{0}/{1:.2f}".format(mi, md), (x - 20, y), cv2.FONT_HERSHEY_SIMPLEX, 0.4, (0, 0, 0))

        coin_count += mi

    return (coin_count, coins_circles)

# NOTE: Vo prosek mu treba okolu 1 sekunda od pocetna slika do vkupna suma so site debug sliki vo proces
def count_coins():
    global coins

    start = time.time()

    # Very important, without sharpening only about half of the coins were found with super pixels
    kernel      = np.array([[-1, -1, -1], [-1, 9, -1], [-1, -1, -1]])
    coins_sharp = cv2.filter2D(coins, cv2.CV_8UC3, kernel)

    coins_lab = cv2.cvtColor(coins_sharp, cv2.COLOR_BGR2LAB)
    coins_hsv = cv2.cvtColor(coins_sharp, cv2.COLOR_BGR2HSV)

    mask, N   = get_superpixels_labels(coins_lab)

    mask = get_dbscan_clusters_mask(mask, N, coins_hsv)

    coins_segmented = cv2.bitwise_and(coins, coins, mask = mask)

    coin_count, coins_circles = count_from_mask(mask)

    end = time.time()

    print ("Counting took {} seconds".format(end - start))

    print ("Total value of coins is " + str(coin_count))

    cv2.imshow("Mask", mask)
    cv2.imshow("Coins segmented", coins_segmented)
    cv2.imshow("Coins circles",   coins_circles)

count_coins()

cv2.imshow("Coins", coins)

cv2.waitKey()