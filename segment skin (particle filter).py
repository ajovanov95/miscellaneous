# Here we try to segment skin pixels based using particle filter with gaussian weights
# The weights are based on the distance from the mean, (h,s,v) that is most color and covariance from it.
# The idea is to sample pixels that are more similar to the skin color pixel until we converge
# Here convergence is defined as having less error than eps
# error is just distance of l2 distances of the mean of pixel hsv values

import cv2
import numpy as np
import numpy.linalg as la


def l1distance(x, mean):
    return np.abs((x[0] / 180.0) * 255 - mean[0]) + np.abs(x[1] - mean[1]) + np.abs(x[2] - mean[2])


def l2distance(x, mean):
    return np.sqrt(((x[0] / 180.0) * 255 - mean[0]) ** 2 + (x[1] - mean[1]) ** 2 + (x[2] - mean[2]) ** 2)


# Sum of L2 distance from mean
def error(particles, image, mean):
    err = 0

    for p in particles:
        pixel = image[p[0], p[1]]

        err += l2distance(pixel, mean)

    return err


def stretch_hist(hist_h):
    out_hist = np.zeros(shape=(256, 1))

    for i in range(0, 180):
        j = int((i / 180.0) * 255)
        out_hist[j] = hist_h[i]

    return np.array(out_hist)


# The returned array has exactly len(array) elements some of which are not unique
def sample_with_replacement(array, weights):
    chosen_indices = np.random.choice(len(array), p=weights, size=len(array), replace=True)
    output = np.empty(shape=array.shape, dtype=np.uint16)

    for j in range(0, len(output)):
        output[j] = array[chosen_indices[j]]

    return output


def bounding_rect(points):
    min_x = 100000  # start with something much higher than expected min
    min_y = 100000
    max_x = -100000  # start with something much lower than expected max
    max_y = -100000

    for item in points:
        if item[0] < min_x:
            min_x = item[0]

        if item[0] > max_x:
            max_x = item[0]

        if item[1] < min_y:
            min_y = item[1]

        if item[1] > max_y:
            max_y = item[1]

    return tuple(np.floor([min_x, min_y, max_x, max_y]).astype(np.uint16))


def run_particle_filter(image, model_image, n_per_row=90, n_per_col=90, max_iter=30, err_threshold=150000):
    model_image = cv2.cvtColor(model_image, cv2.COLOR_BGR2HSV)
    image       = cv2.cvtColor(image, cv2.COLOR_BGR2HSV)

    hist_h = cv2.calcHist([model_image], [0], None, [180], [0, 180])
    hist_s = cv2.calcHist([model_image], [1], None, [256], [0, 256])
    hist_v = cv2.calcHist([model_image], [2], None, [256], [0, 256])

    hist_h = stretch_hist(hist_h)

    ds = np.empty(shape=(3, 256))
    for i in range(0, 256):
        ds[0, i] = hist_h[i]
        ds[1, i] = hist_s[i]
        ds[2, i] = hist_v[i]

    mu    = np.array([np.mean(hist_h), np.mean(hist_s), np.mean(hist_v)])
    sigma = np.cov(ds)

    sigma_det = la.det(sigma)
    sigma_inv = la.inv(sigma)

    const = (2 * np.pi) ** (-1.5) * np.sqrt(sigma_det)

    # x is a (h, s, v) array where h is minmax normalized to be between 0 and 255
    # Multivariate gauss pdf
    pdf = lambda x: const * np.exp(-1 / 2 * (x - mu).T.dot(sigma_inv).dot(x - mu))

    # n_particles = n_per_row * n_per_col approx

    particles = []
    width, height, d = image.shape

    # Initial distribution of the particle filter
    # Uniform over the spectrum of image
    for i in range(0, width, int(width / n_per_col)):
        for j in range(0, height, int(height / n_per_row)):
            particles.append([i, j])

    particles = np.array(particles)

    iterations = 0

    while True:
        err = error(particles, image, mu)

        if err <= err_threshold or iterations >= max_iter:
            break

        weights = []

        for pt in particles:
            [h, s, v] = image[pt[0], pt[1]]
            h = int((h / 180.0) * 255)

            # probability to choose pt
            weights.append(pdf(np.array([h, s, v])))

        # Make sure weights sum to 1
        weights = weights / sum(weights)

        particles = sample_with_replacement(particles, np.array(weights))

        print("Iteration " + str(iterations) + " Error " + str(err))

        iterations += 1

    mask = np.zeros(shape=(image.shape[0], image.shape[1]), dtype=np.uint8)

    for p in particles:
        [i, j] = p
        mask[i, j] = 255

    return [mask, particles]


def test():
    image_model = cv2.imread("ace_lice.png")
    # Ne raboti dobro so ovaa slika
    image       = cv2.imread("ace_lice.png")

    #image       = cv2.resize(image, dsize=(400, 400))

    [mask, parts] = run_particle_filter(image, image_model, image.shape[0] / 2, image.shape[1] / 2, 50)

    cv2.imshow("Mask", mask)
    cv2.imshow("Original", image)

    mask = cv2.dilate(mask, kernel=np.ones(shape=(3, 3)), iterations=4)

    segmented = cv2.bitwise_and(image, image, mask=mask)

    # Use mask from particle filter segmentation
    bgdModel = np.zeros((1, 65), np.float64)
    fgdModel = np.zeros((1, 65), np.float64)

    mask[mask == 0]   = cv2.GC_BGD
    mask[mask == 255] = cv2.GC_FGD #cv2.GC_PR_FGD

    cv2.grabCut(image, mask, None, bgdModel, fgdModel, iterCount=5, mode=cv2.GC_INIT_WITH_MASK)

    mask = np.where((mask == cv2.GC_FGD) | (mask == cv2.GC_PR_FGD), 255, 0).astype(np.uint8)

    gc_segmented = cv2.bitwise_and(image, image, mask=mask)

    for pt in parts:
        cv2.circle(image, (pt[0], pt[1]), 3, [255, 0, 0])

    cv2.imshow("Particles", image)
    cv2.imshow("Segmented", segmented)

    cv2.imshow("GrabCut segmented", gc_segmented)

    cv2.waitKey()

test()
