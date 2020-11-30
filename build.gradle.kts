plugins {
    java
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
	testImplementation("org.junit.jupiter", "junit-jupiter", "5.6.2")
    runtimeOnly("org.junit.jupiter", "junit-jupiter-engine", "5.3.1")
    
}
