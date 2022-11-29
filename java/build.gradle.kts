import com.diffplug.gradle.spotless.SpotlessExtension

plugins {
    java

    // Spotless plugin for checking style of Java code.
    id("com.diffplug.gradle.spotless").version("3.27.2")
}

group = "sjuhosova.aoc"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("com.google.guava", "guava", "11.0.2")

	testImplementation("org.junit.jupiter", "junit-jupiter", "5.6.2")
    runtimeOnly("org.junit.jupiter", "junit-jupiter-engine", "5.3.1")
}

// Configure Spotless plugin for style checking Java code.
configure<SpotlessExtension> {
    java {
        // Use the eclipse formatter format and import order.
        eclipse().configFile(file("eclipse-formatter.xml"))
        importOrderFile(file("$rootDir/importorder.txt"))

        targetExclude("build/")

        // Default added rules.
        paddedCell()
        removeUnusedImports()
        trimTrailingWhitespace()
        endWithNewline()
    }
}
