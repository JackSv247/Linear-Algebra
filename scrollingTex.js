const marquee = document.querySelector('.marquee p');
const marqueeWidth = marquee.offsetWidth; // Get the width of the marquee text
const containerWidth = document.querySelector('.marquee').offsetWidth; // Get the width of the marquee container

// Set the initial position to the right of the container
marquee.style.transform = `translateX(${containerWidth}px)`;

// Define a speed constant (smaller value = faster)
const speed = 0.0001; // Lower this value for faster scrolling
const duration = (marqueeWidth + containerWidth) / speed; // Calculate the total duration based on width and speed

// Set the animation duration
marquee.style.animationDuration = `${duration}s`;

// Function to scroll the marquee
function scrollMarquee() {
    let currentX = containerWidth; // Start position for scrolling

    const scrollInterval = setInterval(() => {
        currentX -= 2; // Move the text to the left (adjust this for speed)
        marquee.style.transform = `translateX(${currentX}px)`; // Update the position

        // Reset position when it moves out of view
        if (currentX < -marqueeWidth) {
            currentX = containerWidth; // Reset to the starting position
        }
    }, 20); // Adjust the speed of the interval (lower = faster)
}

// Start scrolling
scrollMarquee();
